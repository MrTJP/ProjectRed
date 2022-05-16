package mrtjp.projectred.fabrication.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import mrtjp.core.world.WorldLib;
import mrtjp.projectred.core.CoreNetwork;
import mrtjp.projectred.core.tile.IPacketReceiverTile;
import mrtjp.projectred.core.tile.ProjectRedTile;
import mrtjp.projectred.fabrication.block.FabricationBaseBlock;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.IICWorkbenchEditorNetwork;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import net.minecraft.block.BlockState;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static mrtjp.projectred.ProjectRedFabrication.LOGGER;

public class ICWorkbenchTile extends ProjectRedTile implements IPacketReceiverTile, IICWorkbenchEditorNetwork, ITickableTileEntity {

    private final ICWorkbenchEditor editor = new ICWorkbenchEditor(this);

    private final Map<Integer, PacketCustom> editorBufferedStreams = new HashMap<>();
    private final Set<ServerPlayerEntity> playersWatchingScreen = new HashSet<>();

    public ICWorkbenchTile() {
        super(FabricationReferences.IC_WORKBENCH_TILE);
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
    public void saveToNBT(CompoundNBT tag) {
        editor.save(tag);
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
        editor.load(tag);
    }

    private Set<ServerPlayerEntity> filterAndGetWatchers() {
        Set<ServerPlayerEntity> toRemove = playersWatchingScreen.stream()
                .filter(ServerPlayerEntity::hasDisconnected)
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
    public ActionResultType onBlockActivated(PlayerEntity player, Hand hand, BlockRayTraceResult hit) {

        if (!getLevel().isClientSide()) {

            ItemStack stackInHand = player.getItemInHand(hand);

            boolean blueprintOnTable = getBlockState().getValue(FabricationBaseBlock.HAS_BLUEPRINT_PROPERTY);
            boolean blueprintInHand = !stackInHand.isEmpty() && stackInHand.getItem() == FabricationReferences.IC_BLUEPRINT_ITEM;

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

        return ActionResultType.SUCCESS;
    }

    @Override
    public void onBlockRemoved() {
        boolean blueprintOnTable = getBlockState().getValue(FabricationBaseBlock.HAS_BLUEPRINT_PROPERTY);
        if (blueprintOnTable) {
            ItemStack blueprintToDrop = createBlueprintStack();
            WorldLib.dropItem(getLevel(), getBlockPos(), blueprintToDrop);
        }
    }

    private ItemStack createBlueprintStack() {
        ItemStack stack = new ItemStack(FabricationReferences.IC_BLUEPRINT_ITEM);
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
        BlockState newState = getBlockState().setValue(FabricationBaseBlock.HAS_BLUEPRINT_PROPERTY, blueprintOnTable);
        getLevel().setBlockAndUpdate(getBlockPos(), newState);
    }

    private void openGuiFromServer(PlayerEntity player) {
        if (getLevel().isClientSide || !(player instanceof ServerPlayerEntity)) { throw new RuntimeException("Server only"); }
        filterAndGetWatchers().add((ServerPlayerEntity) player);
        LOGGER.info("Watcher added. Size: " + playersWatchingScreen.size());
        sendUpdateToPlayer(0, editor::writeDesc, (ServerPlayerEntity) player);
    }

    private void sendEditorDescription() {

        sendUpdateToPlayerList(50, editor::writeDesc, playersWatchingScreen);
    }

    public void closeGuiFromClient() {
        sendUpdateToServer(1, n -> { });
    }

    @Override
    public void receiveUpdateFromServer(int key, MCDataInput input) {
        switch (key) {
            case 0: // Client opened screen
                editor.readDesc(input);
                ICWorkbenchScreen.openGuiOnClient(this);
                break;
            case 50: // Editor description update
                editor.readDesc(input);
                break;
            case 100: // Some packet for the editor
                receiveBufferedStream(input);
                break;
            default:
                LOGGER.error("Unknown packet key from server: " + key);
        }
    }

    @Override
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayerEntity player) {
        switch (key) {
            case 1: // Client closed screen
                filterAndGetWatchers().remove(player);
                LOGGER.info("Watcher removed. Size: " + playersWatchingScreen.size());
                break;
            case 100: // Some packet for the editor
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
                    CoreNetwork.createPacketReceiverTileServerPacket(this, 100) :
                    CoreNetwork.createPacketReceiverTileClientPacket(this, 100);
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
                for (ServerPlayerEntity player : filterAndGetWatchers()) { packet.sendToPlayer(player); }
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
