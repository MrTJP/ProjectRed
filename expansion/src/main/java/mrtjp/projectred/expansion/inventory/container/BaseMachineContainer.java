package mrtjp.projectred.expansion.inventory.container;

import mrtjp.projectred.core.inventory.container.BasePoweredTileContainer;
import mrtjp.projectred.core.inventory.container.SimpleDataSlot;
import mrtjp.projectred.expansion.tile.BaseMachineTile;
import net.minecraft.world.inventory.MenuType;

import javax.annotation.Nullable;

public abstract class BaseMachineContainer extends BasePoweredTileContainer {

    private final BaseMachineTile tile;

    private int remainingWork;
    private int totalWork;

    public BaseMachineContainer(@Nullable MenuType<?> containerType, int windowId, BaseMachineTile tile) {
        super(containerType, windowId, tile);
        this.tile = tile;

        addDataSlot(new SimpleDataSlot(tile::getRemainingWork, value -> remainingWork = value));
        addDataSlot(new SimpleDataSlot(tile::getTotalWork, value -> totalWork = value));
    }

    public int getProgressScaled(int scale) {
        return totalWork == 0 ? 0 : scale * (totalWork - remainingWork) / totalWork;
    }
}
