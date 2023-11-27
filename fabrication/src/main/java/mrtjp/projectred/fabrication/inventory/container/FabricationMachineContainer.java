package mrtjp.projectred.fabrication.inventory.container;

import mrtjp.projectred.core.inventory.container.BasePoweredTileContainer;
import mrtjp.projectred.core.inventory.container.SimpleDataSlot;
import mrtjp.projectred.fabrication.tile.FabricationMachineTile;
import net.minecraft.world.inventory.MenuType;

import javax.annotation.Nullable;

public class FabricationMachineContainer extends BasePoweredTileContainer {

    private final FabricationMachineTile tile;

    private int remainingWork;
    private int totalWork;

    public FabricationMachineContainer(@Nullable MenuType<?> containerType, int windowId, FabricationMachineTile tile) {
        super(containerType, windowId, tile);
        this.tile = tile;

        addDataSlot(new SimpleDataSlot(tile::getRemainingWork, value -> remainingWork = value));
        addDataSlot(new SimpleDataSlot(tile::getTotalWork, value -> totalWork = value));
    }

    public int getProgressScaled(int scale) {
        return totalWork == 0 ? 0 : scale * (totalWork - remainingWork) / totalWork;
    }
}
