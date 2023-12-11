package mrtjp.projectred.fabrication.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.CoreNetwork;
import mrtjp.projectred.core.tile.IPacketReceiverTile;
import mrtjp.projectred.core.tile.ProjectRedTile;
import mrtjp.projectred.fabrication.block.ICWorkbenchBlock;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.IICWorkbenchEditorNetwork;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.fabrication.init.FabricationBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.init.FabricationItems.IC_BLUEPRINT_ITEM;

public class ICWorkbenchTile extends ProjectRedTile implements IPacketReceiverTile, IICWorkbenchEditorNetwork {

    private static final int KEY_CLIENT_OPENED_SCREEN = 0;
    private static final int KEY_CLIENT_CLOSED_SCREEN = 1;
    private static final int KEY_EDITOR_DESCRIPTION_UPDATE = 50;
    private static final int KEY_EDITOR_PACKET = 100;

    private final ICWorkbenchEditor editor = new ICWorkbenchEditor(this);

    private final Map<Integer, PacketCustom> editorBufferedStreams = new HashMap<>();
    private final Set<ServerPlayer> playersWatchingScreen = new HashSet<>();

    public ICWorkbenchTile(BlockPos pos, BlockState state) {
        super(FabricationBlocks.IC_WORKBENCH_TILE.get(), pos, state);
    }

    public ICWorkbenchEditor getEditor() {
        return editor;
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        editor.writeDesc(out);
    }

    @Override
    public void readDesc(MCDataInput in) {
        editor.readDesc(in);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        editor.save(tag);
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        editor.load(tag);
    }

    private Set<ServerPlayer> filterAndGetWatchers() {
        Set<ServerPlayer> toRemove = playersWatchingScreen.stream()
                .filter(ServerPlayer::hasDisconnected)
                .collect(Collectors.toSet());
        playersWatchingScreen.removeAll(toRemove);
        return playersWatchingScreen;
    }

    @Override
    public void tick() {

        editor.tick();

        flushEditorStreams();
    }

    @Override
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {

        if (!getLevel().isClientSide()) {

            ItemStack stackInHand = player.getItemInHand(hand);

            boolean blueprintOnTable = getBlockState().getValue(ICWorkbenchBlock.BLUEPRINT_PROPERTY);
            boolean blueprintInHand = !stackInHand.isEmpty() && stackInHand.getItem() == IC_BLUEPRINT_ITEM.get();

            if (!blueprintOnTable && blueprintInHand) {
                // load blueprint and activate editor
                editor.readBlueprintTagAndActivate(stackInHand.getTag());
                stackInHand.shrink(1);
                setBlueprintBlockState(true);
                sendEditorDescription();

            } else if (blueprintOnTable && player.isCrouching()) {
                // save/drop blueprint and deactivate editor
                ItemStack blueprintToDrop = createBlueprintStack();
                dropBlueprintStack(blueprintToDrop);
                setBlueprintBlockState(false);
                sendEditorDescription();

            } else {
                // open editor GUI
                openGuiFromServer(player);
            }
        }

        return InteractionResult.SUCCESS;
    }

    @Override
    public void onBlockRemoved() {
        if (getEditor().isActive()) {
            ItemStack blueprintToDrop = createBlueprintStack();
            ProjectRedTile.dropItem(blueprintToDrop, getLevel(), Vector3.fromBlockPos(getBlockPos()));
        }
    }

    private ItemStack createBlueprintStack() {
        ItemStack stack = new ItemStack(IC_BLUEPRINT_ITEM.get());
        editor.writeBlueprintTagAndDeactivate(stack.getOrCreateTag());
        return stack;
    }

    private void dropBlueprintStack(ItemStack blueprintToDrop) {
        BlockPos pos = getBlockPos().offset(0, 1, 0);
        ItemEntity itemEntity = new ItemEntity(getLevel(), pos.getX() + 0.5D, pos.getY() + 0.5D, pos.getZ() + 0.5D, blueprintToDrop);
        itemEntity.setPickUpDelay(10);
        itemEntity.setDeltaMovement(0, 0.15D, 0);
        getLevel().addFreshEntity(itemEntity);
    }

    private void setBlueprintBlockState(boolean blueprintOnTable) {
        BlockState newState = getBlockState().setValue(ICWorkbenchBlock.BLUEPRINT_PROPERTY, blueprintOnTable);
        getLevel().setBlockAndUpdate(getBlockPos(), newState);
    }

    private void openGuiFromServer(Player player) {
        if (getLevel().isClientSide || !(player instanceof ServerPlayer)) { throw new RuntimeException("Server only"); }
        filterAndGetWatchers().add((ServerPlayer) player);
        sendUpdateToPlayer(KEY_CLIENT_OPENED_SCREEN, editor::writeDesc, (ServerPlayer) player);
    }

    private void sendEditorDescription() {

        sendUpdateToPlayerList(KEY_EDITOR_DESCRIPTION_UPDATE, editor::writeDesc, playersWatchingScreen);
    }

    public void closeGuiFromClient() {
        sendUpdateToServer(1, n -> { });
    }

    @Override
    public void receiveUpdateFromServer(int key, MCDataInput input) {
        switch (key) {
            case KEY_CLIENT_OPENED_SCREEN: // Client opened screen
                editor.readDesc(input);
                ICWorkbenchScreen.openGuiOnClient(this);
                break;
            case KEY_EDITOR_DESCRIPTION_UPDATE: // Editor description update
                editor.readDesc(input);
                break;
            case KEY_EDITOR_PACKET: // Some packet for the editor
                receiveBufferedStream(input);
                break;
            default:
                LOGGER.error("Unknown packet key from server: " + key);
        }
    }

    @Override
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayer player) {
        switch (key) {
            case KEY_CLIENT_CLOSED_SCREEN: // Client closed screen
                filterAndGetWatchers().remove(player);
                break;
            case KEY_EDITOR_PACKET: // Some packet for the editor
                receiveBufferedStream(input);
                break;
            default:
                LOGGER.error("Unknown packet key from client: " + key);
        }
    }

    private void receiveBufferedStream(MCDataInput in) {
        int streamKey = in.readUByte();
        int frameKey = in.readUByte();
        while (frameKey != 255) {
            editor.readBufferedStream(in, streamKey, frameKey);
            frameKey = in.readUByte();
        }
    }

    @Override
    public MCDataOutput getBufferedStream(int streamKey, int frameKey) {
        MCDataOutput out =  editorBufferedStreams.computeIfAbsent(streamKey, k -> {
            PacketCustom packet = getLevel().isClientSide ?
                    CoreNetwork.createTileServerPacket(this, (byte) KEY_EDITOR_PACKET) :
                    CoreNetwork.createTileClientPacket(this, (byte) KEY_EDITOR_PACKET);
            packet.writeByte(k); // One-time key that identifies the entire stream
            return packet;
        });

        return out.writeByte(frameKey); // Frame byte inserted between chunks of data
    }

    private void flushEditorStreams() {
        for (PacketCustom packet : editorBufferedStreams.values()) {
            packet.writeByte(255); // Terminator frame byte
            if (getLevel().isClientSide) {
                packet.sendToServer();
            } else {
                for (ServerPlayer player : filterAndGetWatchers()) { packet.sendToPlayer(player); }
            }
        }
        editorBufferedStreams.clear();
    }

    @Override
    public boolean isClientSide() {
        return getLevel().isClientSide();
    }

    @Override
    public void markSave() {
        this.setChanged();
    }

    @Override
    public long getGameTime() {
        return getLevel().getGameTime();
    }
}
