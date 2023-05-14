package mrtjp.projectred.core.inventory.container;

import mrtjp.projectred.core.tile.BasePoweredTile;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.inventory.container.IContainerListener;

import javax.annotation.Nullable;

public class BasePoweredTileContainer extends Container {

    private final BasePoweredTile tile;

    protected int condCharge;
    protected int condFlow;

    public BasePoweredTileContainer(@Nullable ContainerType<?> containerType, int windowId, BasePoweredTile tile) {
        super(containerType, windowId);
        this.tile = tile;
    }

    @Override
    public boolean stillValid(PlayerEntity player) {
        return !tile.isRemoved(); //TODO
    }

    @Override
    public void broadcastChanges() { //TODO switch to Minecraft's Data Slot system
        super.broadcastChanges();

        boolean needsCharge = tile.getConductorCharge() != condCharge;
        boolean needsFlow = tile.getConductorFlow() != condFlow;

        condCharge = tile.getConductorCharge();
        condFlow = tile.getConductorFlow();

        for (IContainerListener listener : containerListeners) {

            if (needsCharge) {
                listener.setContainerData(this, 100, tile.getConductorCharge());
            }

            if (needsFlow) {
                listener.setContainerData(this, 101,tile.getConductorFlow() & 0xFFFF);
                listener.setContainerData(this, 102, tile.getConductorFlow() >> 16 & 0xFFFF);
            }
        }
    }

    @Override
    public void setData(int id, int value) { //TODO switch to Minecraft's Data Slot system
        switch (id) {
            case 100:
                condCharge = value;
                break;
            case 101:
                condFlow = condFlow & 0xFFFF0000 | value & 0xFFFF;
                break;
            case 102:
                condFlow = condFlow & 0xFFFF | (value & 0xFFFF) << 16;
                break;
            default:
                super.setData(id, value);
        }
    }

    public int getChargeScaled(int scale) {
        return Math.min(scale, scale * condCharge / 1000);
    }

    public int getFlowScaled(int scale) {
        return scale * Integer.bitCount(condFlow) / 32;
    }

    public boolean canConductorWork() {
        return condCharge > 600; //TODO This is a render property so we should source this from the tile instead of calculating it here
    }

    public boolean isFlowFull() {
        return condFlow == -1; // TODO same as above
    }
}
