package mrtjp.projectred.expansion;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.api.BlockMover;
import mrtjp.projectred.api.MovementController;
import mrtjp.projectred.api.MovementDescriptor;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.expansion.client.MovingBlockSuppressorRenderer;
import mrtjp.projectred.lib.VecLib;
import net.covers1624.quack.collection.FastStream;
import net.covers1624.quack.util.LazyValue;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.SectionPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.client.ForgeHooksClient;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.client.event.RenderLevelStageEvent;
import net.minecraftforge.client.model.data.EmptyModelData;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.world.ChunkEvent;
import net.minecraftforge.event.world.ChunkWatchEvent;
import net.minecraftforge.event.world.WorldEvent;

import java.util.*;
import java.util.function.Consumer;

import static mrtjp.projectred.expansion.ProjectRedExpansion.LOGGER;

public class MovementManager {

    private static final HashMap<ResourceKey<Level>, MovementManager> SERVER_INSTANCE = new HashMap<>();
    private static final HashMap<ResourceKey<Level>, MovementManager> CLIENT_INSTANCE = new HashMap<>();

    private static final int KEY_BULK_DESC = 0x0;
    private static final int KEY_NEW_STRUCT = 0x1;
    private static final int KEY_EXECUTE_MOVE = 0x2;
    private static final int KEY_CANCEL_MOVE = 0x3;

    private final ResourceKey<Level> dimension;
    private final Map<Integer, MovingStructure> structures = new HashMap<>();
    private final HashMap<ServerPlayer, Set<ChunkPos>> watchingPlayers = new HashMap<>();
    private final HashMap<ServerPlayer, Set<ChunkPos>> newWatchers = new HashMap<>();

    private int nextStructureId = 0;

    public static MovementManager getInstance(Level level) {
        var map = level.isClientSide() ? CLIENT_INSTANCE : SERVER_INSTANCE;
        return map.computeIfAbsent(level.dimension(), MovementManager::new);
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
        getInstance(event.getWorld()).addChunkWatcher(event.getPos(), event.getPlayer());
    }

    public static void onChunkUnwatchEvent(ChunkWatchEvent.UnWatch event) {
        getInstance(event.getWorld()).removeChunkWatcher(event.getPos(), event.getPlayer());
    }

    public static void onChunkUnloadEvent(ChunkEvent.Unload event) {
        if (event.getWorld() instanceof Level level) {
            getInstance(level).cancelMovementsInChunk(level, event.getChunk().getPos());
        }
    }

    public static void onLevelUnload(WorldEvent.Unload event) {
        // Note: Client unloads levels as player changes dimensions, but
        //       server appears to always have all dims loaded and unloads only
        //       on shutdown
        LOGGER.debug("Level {} unloaded", event.getWorld());
    }

    public static void onLevelLoad(WorldEvent.Load event) {
        LOGGER.debug("Level {} loaded", event.getWorld());
    }

    public static void onLevelTick(TickEvent.WorldTickEvent event) {
        if (event.phase == TickEvent.Phase.END) {
            getInstance(event.world).tick(event.world);
        }
    }

    public static void onRenderLevelStage(RenderLevelStageEvent event) {

        Level level = Minecraft.getInstance().level;
        if (level == null) return;

        MovementManager manager = getInstance(level);
        if (manager.structures.isEmpty()) return;

        // Get the renderType for this stage, and skip if we dont care about it
        List<RenderType> renderTypes = List.of(RenderType.solid(), RenderType.cutout(), RenderType.cutoutMipped(), RenderType.translucent());
        RenderType renderType = null;
        for (RenderType type : renderTypes) {
            if (RenderLevelStageEvent.Stage.fromRenderType(type) == event.getStage()) {
                renderType = type;
                break;
            }
        }
        if (renderType == null) return;

        Random random = new Random();

        // Set up camera pose
        Vec3 cam = event.getCamera().getPosition();
        PoseStack stack = event.getPoseStack();
        stack.pushPose();
        stack.translate(-cam.x, -cam.y, -cam.z);

        for (MovingStructure structure : manager.structures.values()) {

            // Set up render offset based on progress of movement
            Vector3 offset = structure.getRenderOffset(event.getPartialTick());
            stack.pushPose();
            stack.translate(offset.x, offset.y, offset.z);

            MultiBufferSource.BufferSource buffers = Minecraft.getInstance().renderBuffers().bufferSource();

            for (MovingRow row : structure.rows) {
                Iterator<BlockPos> it = row.iteratePreMove();
                while (it.hasNext()) {
                    BlockPos p = it.next();
                    BlockState state = level.getBlockState(p);

                    if (!ItemBlockRenderTypes.canRenderInLayer(state, renderType)) continue;

                    // Render the moving block
                    stack.pushPose();
                    stack.translate(p.getX(), p.getY(), p.getZ());

                    RenderType oldType = MinecraftForgeClient.getRenderType();
                    ForgeHooksClient.setRenderType(renderType);

                    MovingBlockSuppressorRenderer.allowMovingRenderOnRenderThread = true;
                    Minecraft.getInstance().getBlockRenderer().renderBatched(state, p, level, stack, buffers.getBuffer(renderType), false, random, EmptyModelData.INSTANCE);
                    MovingBlockSuppressorRenderer.allowMovingRenderOnRenderThread = false;

                    ForgeHooksClient.setRenderType(oldType);
                    stack.popPose(); //p
                }
            }

            buffers.endBatch();
            stack.popPose(); //offset
        }

        stack.popPose(); //cam
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
                sendCancelMove(structure);
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
            LOGGER.debug("Sending descriptions to player {} for {} chunks", player.getName().getString(), posSet.size());
            sendDescriptionsOnWatch(player, posSet);
            // Promote player to a watcher
            watchingPlayers.computeIfAbsent(player, p -> new HashSet<>()).addAll(posSet);
        }
        newWatchers.clear();

        // Execute and remove completed moves
        List<Integer> removed = new LinkedList<>();
        for (var e : structures.entrySet()) {
            MovingStructure structure = e.getValue();
            if (structure.isFinished()) {
                LOGGER.debug("Executing move {}", structure.toString());
                // Execute move
                structure.executeMove(level);
                // Tell client
                sendExecuteMove(structure);
                // Remove
                removed.add(e.getKey());
            }
        }
        for (Integer key : removed) {
            structures.remove(key);
        }
    }

    public MovementDescriptor beginMove(Level level, Set<BlockPos> blocks, int dir, double speed) {

        if (blocks.size() > Configurator.frameMoveLimit) {
            return InternalMovementInfo.failedMovement(blocks.size());
        }

        // Split set of blocks into rows going the opposite direction of the move
        Set<List<BlockPos>> rows = VecLib.resolveRows(blocks, dir^1);

        // Create MovingRows
        List<MovingRow> movingRows = new ArrayList<>(rows.size());
        for (List<BlockPos> row : rows) {
            movingRows.add(new MovingRow(row, dir));
        }

        MovingStructure structure = new MovingStructure(getNextStructureId(), speed, dir, movingRows);
        if (!structure.canMove(level)) return structure;

        // Add structure and begin move
        structures.put(structure.id, structure);
        structure.beginMove(level);

        // Send to client
        sendNewStructureDescription(structure);

        return structure;
    }

    public InternalMovementInfo getMovementInfo(BlockPos pos) {
        for (MovingStructure structure : structures.values()) {
            if (structure.contains(pos)) return structure;
        }
        return InternalMovementInfo.NO_MOVEMENT_INFO;
    }

    //region Network
    private PacketCustom createPacket(int key) {
        return new PacketCustom(ExpansionNetwork.NET_CHANNEL, ExpansionNetwork.MM_FROM_SERVER)
                .writeByte(key);
    }

    public void read(MCDataInput input) {
        int key = input.readUByte();
        switch (key) {
            case KEY_BULK_DESC -> readStructureDescriptions(input);
            case KEY_NEW_STRUCT -> readNewStructure(input);
            case KEY_EXECUTE_MOVE -> readStructureExecution(input);
            case KEY_CANCEL_MOVE -> readStructureCancellation(input);
            default -> LOGGER.warn("Movement manager received unknown key " + key);
        }
    }

    private void readStructureDescriptions(MCDataInput input) {

        int count = input.readUShort();
        for (int i = 0; i < count; i++) {
            MovingStructure structure = MovingStructure.fromDesc(input);
            if (structures.containsKey(structure.id)) {
                LOGGER.debug("Client overwriting existing structure with id {}", structure.id);
            }
            structures.put(structure.id, structure);
        }
    }

    private void readNewStructure(MCDataInput input) {
        MovingStructure structure = MovingStructure.fromDesc(input);

        if (structures.containsKey(structure.id)) {
            LOGGER.debug("Client overwriting existing structure with id {}", structure.id);
        }

        structures.put(structure.id, structure);
        structure.beginMove(Minecraft.getInstance().level);
    }

    private void readStructureExecution(MCDataInput input) {
        // Generate a new structure in case it was stale on client
        MovingStructure structure = MovingStructure.fromDesc(input);

        // Execute move
        structure.executeMove(Minecraft.getInstance().level);

        // Delete struct that *should* have been on the client
        if (structures.remove(structure.id) == null) {
            LOGGER.warn("Move executed for unknown structure id {}", structure.id);
        }
    }

    private void readStructureCancellation(MCDataInput input) {
        int id = input.readUShort();
        MovingStructure structure = structures.get(id);
        if (structure == null) {
            LOGGER.debug("Received cancellation for unknown structure id {}", id);
        } else {
            structure.cancelMove(Minecraft.getInstance().level);
            structures.remove(id);
        }
    }

    // Send initial description of structures that player
    private void sendDescriptionsOnWatch(ServerPlayer player, Set<ChunkPos> posSet) {
        Collection<MovingStructure> structs = getStructuresIntersectingChunks(posSet);
        if (structs.isEmpty()) return;

        PacketCustom packet = createPacket(KEY_BULK_DESC);

        // Write structs
        packet.writeShort(structs.size());
        for (MovingStructure s : structs) {
            s.writeDesc(packet);
        }
        packet.sendToPlayer(player);
    }

    private void sendNewStructureDescription(MovingStructure structure) {
        PacketCustom packet = createPacket(KEY_NEW_STRUCT);

        // Write struct
        structure.writeDesc(packet);

        // Send to interested players
        for (ServerPlayer player : playersWatchingStructure(structure)) {
            packet.sendToPlayer(player);
        }
    }

    private void sendExecuteMove(MovingStructure structure) {
        PacketCustom packet = createPacket(KEY_EXECUTE_MOVE);
        structure.writeDesc(packet);

        for (ServerPlayer player : playersWatchingStructure(structure)) {
            packet.sendToPlayer(player);
        }
    }

    private void sendCancelMove(MovingStructure structure) {
        PacketCustom packet = createPacket(KEY_CANCEL_MOVE);
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

    private static class MovingStructure implements InternalMovementInfo {
        public final int id;

        private final double speed;
        private final int dir;
        private final List<MovingRow> rows;
        private final int totalSize;

        private final LazyValue<HashSet<ChunkPos>> intersectingChunks = new LazyValue<>(this::computeIntersectingChunks);
        private final LazyValue<HashSet<SectionPos>> renderChunks = new LazyValue<>(this::computeRenderChunks);

        private boolean started = false;
        private boolean cancelled = false;
        private double progress;

        public MovingStructure(int id, double speed, int dir, List<MovingRow> rows, double progress) {
            this.id = id;
            this.speed = speed;
            this.dir = dir;
            this.rows = Collections.unmodifiableList(rows);
            this.progress = progress;
            this.totalSize = FastStream.of(rows).intSum(r -> r.size);
        }

        public MovingStructure(int id, double speed, int dir, List<MovingRow> rows) {
            this(id, speed, dir, rows, 0D);
        }

        //region Network
        public void writeDesc(MCDataOutput output) {
            output.writeShort(id);
            output.writeDouble(speed);
            output.writeByte(dir);
            output.writeShort(rows.size());
            for (MovingRow row : rows) {
                output.writePos(row.pos);
                output.writeShort(row.size);
            }
            output.writeDouble(progress); //TODO use integers instead
        }

        public static MovingStructure fromDesc(MCDataInput input) {
            int id = input.readUShort();
            double speed = input.readDouble();
            int dir = input.readUByte();
            int size = input.readUShort();
            List<MovingRow> rows = new ArrayList<>(size);
            for (int i = 0; i < size; i++) {
                rows.add(new MovingRow(input.readPos(), dir, input.readUShort()));
            }
            double progress = input.readDouble();

            return new MovingStructure(id, speed, dir, rows, progress);
        }
        //endregion

        //region Movement description
        @Override
        public MovementStatus getStatus() {
            if (!started) return MovementStatus.FAILED;
            if (cancelled) return MovementStatus.CANCELLED;
            if (progress >= 1D) return MovementStatus.FINISHED;
            return MovementStatus.MOVING;
        }

        @Override
        public boolean isMoving() {
            return getStatus() == MovementStatus.MOVING;
        }

        @Override
        public double getProgress() {
            return progress;
        }

        @Override
        public int getSize() {
            return totalSize;
        }

        @Override
        public Vector3 getRenderOffset(float partialTicks) {
            return Vector3.fromBlockPos(BlockPos.ZERO.relative(Direction.values()[dir])).multiply(progress + speed * partialTicks);
        }
        //endregion

        public HashSet<ChunkPos> getChunkSet() {
            return intersectingChunks.get();
        }

        public boolean intersects(ChunkPos pos) {
            return intersectingChunks.get().contains(pos);
        }

        public boolean contains(BlockPos pos) {
            for (MovingRow row : rows) {
                if (row.contains(pos)) return true;
            }
            return false;
        }

        public void tickProgress(Level level) {
            progress = Math.min(progress + speed, 1D);
            FastStream.of(rows).forEach(r -> r.pushEntities(level, progress));
        }

        public boolean isFinished() {
            return progress >= 1D;
        }

        public boolean canMove(Level level) {
            for (MovingRow row : rows) {
                if (!row.canMove(level)) return false;
            }
            return true;
        }

        public void beginMove(Level level) {
            started = true;
            FastStream.of(rows).forEach(r -> r.beginMove(level));

            if (level.isClientSide) {
                // Force chunk to re-render so rendering of moving block can be suppressed by MovingBlockSuppressorRenderer
                markChunksForRender();
            }
        }

        public void executeMove(Level level) {

            // Run movement phases
            FastStream.of(rows).forEach(r -> r.moveBlocks(level));
            FastStream.of(rows).forEach(r -> r.postMove(level));
            FastStream.of(rows).forEach(r -> r.endMove(level));

            // Update neighbors
            Set<BlockPos> changes = new HashSet<>();
            FastStream.of(rows).forEach(r -> r.addNeighborChanges(level, changes));

            for (BlockPos pos : changes) {
                BlockState state = level.getBlockState(pos);
                state.updateNeighbourShapes(level, pos, 0, 0);
                state.updateIndirectNeighbourShapes(level, pos, 0, 0);
                level.neighborChanged(pos, Blocks.AIR, pos); //TODO use better context here
            }

            // Update lighting
            markBlocksForLightUpdate(level);

            // Update chunk rendering
            if (level.isClientSide) {
                markChunksForRender();
            }

            //TODO Tick rescheduling
        }

        public void cancelMove(Level level) {
            // Shouldn't need to do anything. Nothing happens until the animation is finished
            cancelled = true;
        }

        private void markChunksForRender() {
            FastStream.of(renderChunks.get()).forEach(p -> Minecraft.getInstance().levelRenderer.setSectionDirty(p.x(), p.y(), p.z(), true));
        }

        private void markBlocksForLightUpdate(Level level) {
            FastStream.of(rows).forEach(r -> r.forEachAll(p -> level.getLightEngine().checkBlock(p)));
        }

        private HashSet<ChunkPos> computeIntersectingChunks() {
            HashSet<ChunkPos> chunks = new HashSet<>();
            FastStream.of(rows).forEach(r -> r.forEachAll(p -> chunks.add(new ChunkPos(p))));
            return chunks;
        }

        private HashSet<SectionPos> computeRenderChunks() {
            HashSet<SectionPos> chunks = new HashSet<>();
            FastStream.of(rows).forEach(r -> r.forEachAll(p -> {
                // Add all neighbors of blocks as well to update culled faces
                for (int s = 0; s < 6; s++) {
                    chunks.add(SectionPos.of(p.relative(Direction.values()[s])));
                }
                // Note: no need to add position itself, as it *must* be in one of above chunks
            }));
            return chunks;
        }

        @Override
        public String toString() {
            return "MovingStructure{" +
                    "id=" + id +
                    ", speed=" + speed +
                    ", dir=" + dir +
                    ", progress=" + progress +
                    ", rows=" + rows +
                    '}';
        }
    }

    private static final class MovingRow {

        public final BlockPos pos;
        public final int dir;
        public final int size;

        private MovingRow(BlockPos pos, int dir, int size) {
            this.pos = pos;
            this.dir = dir;
            this.size = size;
        }

        private MovingRow(List<BlockPos> row, int dir) {
            // Row's head should be the next block towards dir where everything will move,
            // then followed by the rest of the row
            this.pos = row.get(0).relative(Direction.values()[dir]);
            this.dir = dir;
            this.size = row.size() + 1;
        }

        public boolean contains(BlockPos pos) {

            BlockPos p1 = VecLib.projectDir(this.pos, dir);
            BlockPos p2 = VecLib.projectDir(pos, dir);

            // If projections towards dir plane are not equal, they cannot be on same axis
            if (!p1.equals(p2)) return false;

            // pos is on the same line as this row. Check if its between start and end
            int a1 = VecLib.rejectComponent(this.pos, dir);
            int a2 = VecLib.rejectComponent(this.pos.relative(Direction.values()[dir ^ 1], size - 1), dir);
            int b = VecLib.rejectComponent(pos, dir);

            return Math.min(a1, a2) <= b && b <= Math.max(a1, a2);
        }

        public boolean canMove(Level level) {
            if (!level.isLoaded(pos)) return false;
            BlockState state = level.getBlockState(pos);
            if (!(state.isAir() || state.getMaterial().isReplaceable())) return false;

            Iterator<BlockPos> it = iteratePreMove();
            while (it.hasNext()) {
                BlockPos pos = it.next();
                MovementController controller = MovementRegistry.getMovementController(level, pos);
                // Allow hooks to conditionally block movement
                if (controller != null && !controller.isMovable(level, pos, Direction.values()[dir])) return false;
            }

            return true;
        }

        public void beginMove(Level level) {
            //TODO spawn movement blocks

            if (!level.isClientSide) {
                // Notify blocks/BEs conforming to MovementController about move
                forEachPreMove(p -> {
                    MovementController controller = MovementRegistry.getMovementController(level, p);
                    if (controller != null) controller.onMovementStarted(level, p, Direction.values()[dir]);
                });
            }
        }

        public void pushEntities(Level level, double progress) {
            //TODO
        }

        public void moveBlocks(Level level) {
            forEachPreMove(p -> {
                BlockMover mover = MovementRegistry.getMover(level, p);
                mover.move(level, p, Direction.values()[dir]);
            });
        }

        public void postMove(Level level) {
            forEachPostMove(p -> {
                BlockMover mover = MovementRegistry.getMover(level, p);
                mover.postMove(level, p);
            });
        }

        public void endMove(Level level) {
            if (!level.isClientSide) {
                forEachPostMove(p -> {
                    MovementController controller = MovementRegistry.getMovementController(level, pos);
                    if (controller != null) controller.onMovementFinished(level, pos);
                });
            }
        }

        public void addNeighborChanges(Level level, Set<BlockPos> changes) {
            forEachAll(p -> {
                changes.add(p);
                for (int s = 0; s < 6; s++) {
                    changes.add(p.relative(Direction.values()[s]));
                }
            });
        }

        private RowIterator iteratePreMove() {
            return new RowIterator(1, size);
        }

        private RowIterator iteratePostMove() {
            return new RowIterator(0, size - 1);
        }

        private RowIterator iterateAll() {
            return new RowIterator(0, size);
        }

        private void forEachPreMove(Consumer<BlockPos> action) {
            Iterator<BlockPos> it = iteratePreMove();
            while (it.hasNext()) {
                action.accept(it.next());
            }
        }

        private void forEachPostMove(Consumer<BlockPos> action) {
            Iterator<BlockPos> it = iteratePostMove();
            while (it.hasNext()) {
                action.accept(it.next());
            }
        }

        private void forEachAll(Consumer<BlockPos> action) {
            Iterator<BlockPos> it = iterateAll();
            while (it.hasNext()) {
                action.accept(it.next());
            }
        }

        @Override
        public String toString() {
            return "MovingRow[" +
                    "pos={" + pos.getX() + ", " + pos.getY() + ", " + pos.getZ() + "}" +
                    ", size=" + size + ']';
        }

        private class RowIterator implements Iterator<BlockPos> {

            private final int size;
            private final BlockPos.MutableBlockPos mpos = new BlockPos.MutableBlockPos();
            private int i;

            public RowIterator(int start, int size) {
                this.size = size;
                this.i = start;
            }

            @Override
            public boolean hasNext() {
                return i < size;
            }

            @Override
            public BlockPos next() {
                return mpos.set(pos).move(Direction.values()[dir].getOpposite(), i++);
            }
        }
    }

    public interface InternalMovementInfo extends MovementDescriptor {

        InternalMovementInfo NO_MOVEMENT_INFO = new InternalMovementInfo() {
            //@formatter:off
            @Override public Vector3 getRenderOffset(float partialTicks) { return Vector3.ZERO; }
            @Override public MovementStatus getStatus() { return MovementStatus.UNKNOWN; }
            @Override public boolean isMoving() { return false; }
            @Override public double getProgress() { return 0; }
            @Override public int getSize() { return 0; }
            //@formatter:on
        };

        private static InternalMovementInfo failedMovement(int size) {
            return new InternalMovementInfo() {
                //@formatter:off
                @Override public Vector3 getRenderOffset(float partialTicks) { return Vector3.ZERO; }
                @Override public MovementStatus getStatus() { return MovementStatus.FAILED; }
                @Override public boolean isMoving() { return false; }
                @Override public double getProgress() { return 0; }
                @Override public int getSize() { return size; }
                //@formatter:on
            };
        }

        Vector3 getRenderOffset(float partialTicks);
    }
}
