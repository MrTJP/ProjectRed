package mrtjp.projectred.core.inventory.container;

import mrtjp.projectred.core.tile.BasePoweredBlockEntity;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;

import javax.annotation.Nullable;

public abstract class BasePoweredBlockEntityMenu extends AbstractContainerMenu {

    private final BasePoweredBlockEntity tile;

    protected int condCharge;
    protected int condFlow;

    public BasePoweredBlockEntityMenu(@Nullable MenuType<?> containerType, int windowId, BasePoweredBlockEntity tile) {
        super(containerType, windowId);
        this.tile = tile;

        addDataSlot(new SimpleDataSlot(tile::getConductorCharge, value -> condCharge = value));
        addDataSlot(new SimpleDataSlot(() -> tile.getConductorFlow() & 0xFFFF, value -> condFlow = condFlow & 0xFFFF0000 | value & 0xFFFF));
        addDataSlot(new SimpleDataSlot(() -> tile.getConductorFlow() >> 16 & 0xFFFF, value -> condFlow = condFlow & 0xFFFF | value << 16));
    }

    @Override
    public boolean stillValid(Player pPlayer) {
        return Container.stillValidBlockEntity(tile, pPlayer);
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
