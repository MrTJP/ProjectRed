package mrtjp.projectred.expansion.inventory.container;

import mrtjp.projectred.core.inventory.container.BasePoweredTileContainer;
import mrtjp.projectred.expansion.tile.BaseMachineTile;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.inventory.container.IContainerListener;

import javax.annotation.Nullable;

public abstract class BaseMachineContainer extends BasePoweredTileContainer {

    private final BaseMachineTile tile;

    private int remainingWork;
    private int totalWork;

    public BaseMachineContainer(@Nullable ContainerType<?> containerType, int windowId, BaseMachineTile tile) {
        super(containerType, windowId, tile);
        this.tile = tile;
    }

    @Override
    public void broadcastChanges() {
        super.broadcastChanges();

        boolean needsRemainingWork = remainingWork != tile.getRemainingWork();
        boolean needsTotalWork = totalWork != tile.getTotalWork();

        remainingWork = tile.getRemainingWork();
        totalWork = tile.getTotalWork();

        for (IContainerListener listener : containerListeners) {
            if (needsRemainingWork) {
                listener.setContainerData(this, 110, remainingWork);
            }
            if (needsTotalWork) {
                listener.setContainerData(this, 111, totalWork);
            }
        }
    }

    @Override
    public void setData(int id, int value) {
        switch (id) {
            case 110:
                remainingWork = value;
                break;
            case 111:
                totalWork = value;
                break;
            default:
                super.setData(id, value);
        }
    }

    public int getProgressScaled(int scale) {
        return totalWork == 0 ? 0 : scale * (totalWork - remainingWork) / totalWork;
    }
}
