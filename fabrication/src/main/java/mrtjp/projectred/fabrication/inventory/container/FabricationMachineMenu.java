package mrtjp.projectred.fabrication.inventory.container;

import mrtjp.projectred.core.inventory.container.BasePoweredBlockEntityMenu;
import mrtjp.projectred.core.inventory.container.SimpleDataSlot;
import mrtjp.projectred.fabrication.tile.FabricationMachineBlockEntity;
import net.minecraft.world.inventory.MenuType;

import javax.annotation.Nullable;

public abstract class FabricationMachineMenu extends BasePoweredBlockEntityMenu {

    private final FabricationMachineBlockEntity tile;

    private int remainingWork;
    private int totalWork;

    public FabricationMachineMenu(@Nullable MenuType<?> containerType, int windowId, FabricationMachineBlockEntity tile) {
        super(containerType, windowId, tile);
        this.tile = tile;

        addDataSlot(new SimpleDataSlot(tile::getRemainingWork, value -> remainingWork = value));
        addDataSlot(new SimpleDataSlot(tile::getTotalWork, value -> totalWork = value));
    }

    public int getProgressScaled(int scale) {
        return totalWork == 0 ? 0 : scale * (totalWork - remainingWork) / totalWork;
    }
}
