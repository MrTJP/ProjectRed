package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import javax.annotation.Nullable;

public abstract class BaseConnectableBlockEntity extends ProjectRedBlockEntity implements IConnectableBlockEntity, IPacketReceiverBlockEntity {

    public static final int PACKET_CONN_MAP = 2;

    private long connMap = 0L;

    public BaseConnectableBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    @Override
    public long getConnMap() {
        return connMap;
    }

    @Override
    public void setConnMap(long connMap) {
        this.connMap = connMap;
    }

    @Override
    public void saveToNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        tag.putLong("connMap", connMap);
    }

    @Override
    public void loadFromNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        connMap = tag.getLong("connMap");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        out.writeLong(connMap);
    }

    @Override
    public void readDesc(MCDataInput in) {
        connMap = in.readLong();
    }

    @Override
    public void receiveUpdateFromServer(int key, MCDataInput input) {
        if (key == PACKET_CONN_MAP) {
            connMap = input.readLong();
        }
    }

    @Override
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayer player) {
        // NO-OP
    }

    protected boolean clientNeedsMask() {
        return false;
    }

    protected void sendConnUpdate() {
        if (clientNeedsMask()) {
            sendUpdateToPlayersWatchingChunk(PACKET_CONN_MAP, out -> out.writeLong(connMap));
        }
    }

    @Override
    public void onMaskChanged() {
        sendConnUpdate();
    }

    @Override
    public void onNeighborBlockChanged(BlockPos neighborPos) {
        super.onNeighborBlockChanged(neighborPos);
        if (!getBlockLevel().isClientSide) {
            updateExternals();
        }
    }

    @Override
    public void onBlockPlaced(@Nullable LivingEntity player, ItemStack item) {
        super.onBlockPlaced(player, item);
        if (!getBlockLevel().isClientSide) {
            updateExternals();
        }
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        if (!getBlockLevel().isClientSide) {
            notifyConnectedExternals();
        }
    }
}
