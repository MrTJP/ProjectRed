package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.TileEntityType;
import net.minecraft.util.math.BlockPos;

public abstract class BaseConnectableTile extends ProjectRedTile implements IConnectableTile, IPacketReceiverTile {

    public static final int PACKET_CONN_MAP = 2;

    private long connMap = 0L;

    public BaseConnectableTile(TileEntityType<?> type) {
        super(type);
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
    public void saveToNBT(CompoundNBT tag) {
        tag.putLong("connMap", connMap);
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
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
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayerEntity player) {
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
    public void onBlockPlaced(LivingEntity player, ItemStack item) {
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
