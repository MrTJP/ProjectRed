package mrtjp.projectred.expansion;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.api.MovementDescriptor;
import mrtjp.projectred.core.Configurator;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.event.level.ChunkEvent;
import net.neoforged.neoforge.event.level.ChunkWatchEvent;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.neoforged.neoforge.event.tick.LevelTickEvent;

import javax.annotation.Nullable;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import static mrtjp.projectred.api.MovementDescriptor.MovementStatus.MOVING;
import static mrtjp.projectred.api.MovementDescriptor.MovementStatus.PENDING_FINALIZATION;
import static mrtjp.projectred.expansion.ProjectRedExpansion.LOGGER;

public class MovementManager {

    private static final IdentityHashMap<ResourceKey<Level>, MovementManager> SERVER_INSTANCE = new IdentityHashMap<>();
    private static final IdentityHashMap<ResourceKey<Level>, MovementManager> CLIENT_INSTANCE = new IdentityHashMap<>();

    private static final int KEY_BULK_DESC = 0x0;
    private static final int KEY_NEW_STRUCT = 0x1;
    private static final int KEY_EXECUTE_MOVE = 0x2;
    private static final int KEY_CANCEL_MOVE = 0x3;

    private final ResourceKey<Level> dimension;
    private final Map<Integer, MovingStructure> structures = new ConcurrentHashMap<>();
    private final HashMap<ServerPlayer, Set<ChunkPos>> watchingPlayers = new HashMap<>();
    private final HashMap<ServerPlayer, Set<ChunkPos>> newWatchers = new HashMap<>();

    private int nextStructureId = 0;

    public static MovementManager getInstance(Level level) {
        var map = level.isClientSide() ? CLIENT_INSTANCE : SERVER_INSTANCE;
        return map.computeIfAbsent(level.dimension(), MovementManager::new);
    }

    public static @Nullable MovementManager getClientInstanceNullable() {
        var clientLevel = Minecraft.getInstance().level;
        if (clientLevel == null) {
           return null;
        }
        return CLIENT_INSTANCE.get(clientLevel.dimension());
    }

    public static @Nullable MovementManager getInstanceNullable(Level level) {
        var map = level.isClientSide() ? CLIENT_INSTANCE : SERVER_INSTANCE;
        return map.get(level.dimension());
    }

    public MovementManager(ResourceKey<Level> dimension) {
        this.dimension = dimension;
        LOGGER.debug("Created MovementManager for dimension {}", dimension.location());
    }

    private int getNextStructureId() {
        int next = nextStructureId;
        nextStructureId = (nextStructureId + 1) % Short.MAX_VALUE;
        return next;
    }

    //region Events
    public static void onChunkWatchEvent(ChunkWatchEvent.Watch event) {
        getInstance(event.getLevel()).addChunkWatcher(event.getPos(), event.getPlayer());
    }

    public static void onChunkUnwatchEvent(ChunkWatchEvent.UnWatch event) {
        getInstance(event.getLevel()).removeChunkWatcher(event.getPos(), event.getPlayer());
    }

    public static void onChunkUnloadEvent(ChunkEvent.Unload event) {
//        LOGGER.debug("Chunk {} unloaded", event.getLevel());
        if (event.getLevel() instanceof Level level) {
            getInstance(level).cancelMovementsInChunk(level, event.getChunk().getPos());
        }
    }

    public static void onLevelUnload(LevelEvent.Unload event) {
        // Note: Client unloads levels as player changes dimensions or leaves server, but
        //       server appears to always have all dims loaded and unloads only
        //       on shutdown
        LOGGER.debug("Level {} unloaded", event.getLevel());
        if (event.getLevel() instanceof Level level) {
            getInstance(level).cancelMovementsOnUnload(level);
        }
    }

    public static void onLevelLoad(LevelEvent.Load event) {
        LOGGER.debug("Level {} loaded", event.getLevel());
    }

    public static void onLevelTick(LevelTickEvent.Post event) {
        getInstance(event.getLevel()).tick(event.getLevel());
    }

    private void addChunkWatcher(ChunkPos pos, ServerPlayer player) {
//        LOGGER.debug("Player {} started watching chunk {},{} (isClient: {})", player.getName().getString(), pos.x, pos.z, player.level.isClientSide);
        newWatchers.computeIfAbsent(player, p -> new HashSet<>()).add(pos);
    }

    private void removeChunkWatcher(ChunkPos pos, ServerPlayer player) {
//        LOGGER.debug("Player {} stopped watching chunk {},{} (isClient: {})", player.getName().getString(), pos.x, pos.z, player.level.isClientSide);
        Set<ChunkPos> newWatchersSet = newWatchers.get(player);
        if (newWatchersSet != null) newWatchersSet.remove(pos);

        Set<ChunkPos> watchingPlayersSet = watchingPlayers.get(player);
        if (watchingPlayersSet != null) watchingPlayersSet.remove(pos);
    }

    private void cancelMovementsOnUnload(Level level) {
        // Note: Call this on both sides
        LOGGER.debug("Cancelling {} movements on level {} unload", structures.size(), level);
        for (var structure : structures.values()) {
            structure.cancelMove(level); // prob doesn't matter at this point
        }
        structures.clear();
        nextStructureId = 0;
    }

    private void cancelMovementsInChunk(Level level, ChunkPos pos) {
        if (level.isClientSide) {
            return;
        }
        List<Integer> removed = new LinkedList<>();
        for (MovingStructure structure : getStructuresIntersectingChunks(Collections.singletonList(pos))) {
            if (structure.intersects(pos)) {
                LOGGER.debug("Cancelling move {}", structure.toString());
                // Cancel move
                structure.cancelMove(level);
                // Tell client
                sendCancelMove(structure, level);
                // Remove
                removed.add(structure.id);
            }
        }
        for (Integer key : removed) {
            structures.remove(key);
        }
    }

    private void tick(Level level) {

        // Tick progress of all movements (client and server side)
        for (var e : structures.entrySet()) {
            e.getValue().tickProgress(level);
        }

        if (level.isClientSide) return;

        // Send descriptions to new watchers
        for (var e : newWatchers.entrySet()) {
            ServerPlayer player = e.getKey();
            Set<ChunkPos> posSet = e.getValue();
//            LOGGER.debug("Sending descriptions to player {} for {} chunks", player.getName().getString(), posSet.size());
            sendDescriptionsOnWatch(player, posSet);
            // Promote player to a watcher
            watchingPlayers.computeIfAbsent(player, p -> new HashSet<>()).addAll(posSet);
        }
        newWatchers.clear();

        // Execute and remove completed moves
        List<Integer> removed = new LinkedList<>();
        for (var e : structures.entrySet()) {
            MovingStructure structure = e.getValue();
            if (structure.getStatus() == PENDING_FINALIZATION) {
                LOGGER.debug("Executing move {}", structure.toString());

                // Execute pre-move, which does silent block modifications
                structure.executePreMove(level);

                // Tell client to execute both pre-move and post-move
                sendExecuteMove(structure, level);

                // Execute post-move. Block updates can be done here since client has moved blocks already
                structure.executePostMove(level);

                // Remove
                removed.add(e.getKey());
            }
        }
        for (Integer key : removed) {
            structures.remove(key);
        }
    }

    public MovementDescriptor beginMove(Level level, Set<BlockPos> blocks, int dir, double speed) {

        if (blocks.size() > Configurator.SERVER.frameMoveLimit.get()) {
            return MovingStructureInfo.failedMovement(blocks.size());
        }

        MovingStructure structure = MovingStructure.fromBlockSet(getNextStructureId(), speed, dir, blocks);
        if (!structure.canMove(level)) return structure;

        // Add structure and send to client
        structures.put(structure.id, structure);
        sendNewStructureDescription(structure, level);

        // Begin move (client does this when structure received from above call)
        structure.beginMove(level);

        return structure;
    }

    public boolean hasNoMovingStructures() {
        return structures.isEmpty();
    }

    public Collection<MovingStructure> getMovingStructures() {
        return structures.values();
    }

    public MovingStructureInfo getMovementInfo(BlockPos pos) {
        for (MovingStructure structure : structures.values()) {
            if (structure.contains(pos)) return structure;
        }
        return MovingStructureInfo.NO_MOVEMENT_INFO;
    }

    //region Network
    private PacketCustom createPacket(int key, RegistryAccess registryAccess) {
        return new PacketCustom(ExpansionNetwork.NET_CHANNEL, ExpansionNetwork.MM_FROM_SERVER, registryAccess)
                .writeByte(key);
    }

    public void read(MCDataInput input, Level level) {
        int key = input.readUByte();
        switch (key) {
            case KEY_BULK_DESC -> readStructureDescriptions(input, level);
            case KEY_NEW_STRUCT -> readNewStructure(input, level);
            case KEY_EXECUTE_MOVE -> readStructureExecution(input, level);
            case KEY_CANCEL_MOVE -> readStructureCancellation(input, level);
            default -> LOGGER.warn("Movement manager received unknown key " + key);
        }
    }

    private void readStructureDescriptions(MCDataInput input, Level level) {

        int count = input.readUShort();
        for (int i = 0; i < count; i++) {
            MovingStructure structure = MovingStructure.fromDesc(input);
            if (structures.containsKey(structure.id)) {
                LOGGER.debug("Client overwriting existing structure with id {}", structure.id);
            }
            structures.put(structure.id, structure);
        }
    }

    private void readNewStructure(MCDataInput input, Level level) {
        MovingStructure structure = MovingStructure.fromDesc(input);

        if (structures.containsKey(structure.id)) {
            LOGGER.debug("Client overwriting existing structure with id {}", structure.id);
        }

        structures.put(structure.id, structure);
        structure.beginMove(level);
    }

    private void readStructureExecution(MCDataInput input, Level level) {
        int id = input.readUShort();
        var structure = structures.get(id);

        // Client would have received this structure already
        if (structure == null) {
            LOGGER.error("Pre-move executed for unknown structure id {}. Adding it for post-move.", id);
            return;
        }

        assert structure.getStatus() == MOVING || structure.getStatus() == PENDING_FINALIZATION;

        // The client is usually behind by some ticks. Tick progress rapidly to complete the move.
        // TODO Add tickProgressToEnd() method. Pushing entities is more efficient if done all at
        int ticksBehind = 0;
        while (structure.getStatus() == MOVING) {
            structure.tickProgress(level);
            ticksBehind++;
        }
        if (ticksBehind > 1) {
            LOGGER.warn("Client structure with id {} was {} ticks behind!", id, ticksBehind);
        }

        // Execute pre-move and post-move operations. Server has only done pre-move so far.
        // It will do post-move after the client.
        structure.executePreMove(level);
        structure.executePostMove(level);

        // Full movement complete on client-side. Remove structure
        structures.remove(id);
    }

    private void readStructureCancellation(MCDataInput input, Level level) {
        int id = input.readUShort();
        MovingStructure structure = structures.get(id);
        if (structure == null) {
            LOGGER.debug("Received cancellation for unknown structure id {}", id);
        } else {
            structure.cancelMove(level);
            structures.remove(id);
        }
    }

    // Send initial description of structures that player
    private void sendDescriptionsOnWatch(ServerPlayer player, Set<ChunkPos> posSet) {
        Collection<MovingStructure> structs = getStructuresIntersectingChunks(posSet);
        if (structs.isEmpty()) return;

        PacketCustom packet = createPacket(KEY_BULK_DESC, player.registryAccess());

        // Write structs
        packet.writeShort(structs.size());
        for (MovingStructure s : structs) {
            s.writeDesc(packet);
        }
        packet.sendToPlayer(player);
    }

    private void sendNewStructureDescription(MovingStructure structure, Level level) {
        PacketCustom packet = createPacket(KEY_NEW_STRUCT, level.registryAccess());

        // Write struct
        structure.writeDesc(packet);

        // Send to interested players
        for (ServerPlayer player : playersWatchingStructure(structure)) {
            packet.sendToPlayer(player);
        }
    }

    private void sendExecuteMove(MovingStructure structure, Level level) {
        PacketCustom packet = createPacket(KEY_EXECUTE_MOVE, level.registryAccess());
        packet.writeShort(structure.id);

        for (ServerPlayer player : playersWatchingStructure(structure)) {
            packet.sendToPlayer(player);
        }
    }

    private void sendCancelMove(MovingStructure structure, Level level) {
        PacketCustom packet = createPacket(KEY_CANCEL_MOVE, level.registryAccess());
        packet.writeShort(structure.id);

        for (ServerPlayer player : playersWatchingStructure(structure)) {
            packet.sendToPlayer(player);
        }
    }
    //endregion

    //region Utilities
    private Collection<MovingStructure> getStructuresIntersectingChunks(Collection<ChunkPos> chunks) {
        List<MovingStructure> structures = new LinkedList<>();
        for (MovingStructure structure : this.structures.values()) {
            for (ChunkPos pos : chunks) {
                if (structure.intersects(pos)) structures.add(structure);
            }
        }
        return structures;
    }

    private Collection<ServerPlayer> playersWatchingStructure(MovingStructure structure) {
        List<ServerPlayer> players = new LinkedList<>();

        Set<ChunkPos> chunks = structure.getChunkSet();
        for (var e : watchingPlayers.entrySet()) {
            for (ChunkPos pos : chunks) {
                if (e.getValue().contains(pos)) {
                    players.add(e.getKey());
                    break;
                }
            }
        }
        return players;
    }
    //endregion

}
