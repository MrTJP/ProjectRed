package mrtjp.projectred.expansion.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.core.inventory.container.BasePoweredBlockEntityMenu;
import mrtjp.projectred.core.inventory.container.SimpleDataSlot;
import mrtjp.projectred.expansion.init.ExpansionMenus;
import mrtjp.projectred.expansion.item.IChargable;
import mrtjp.projectred.expansion.tile.ChargingBenchBlockEntity;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntity;

public class ChargingBenchMenu extends BasePoweredBlockEntityMenu {

    public static final ICCLContainerFactory<ChargingBenchMenu> FACTORY = (windowId, inventory, packet) -> {
        BlockEntity tile = inventory.player.level().getBlockEntity(packet.readPos());
        if (!(tile instanceof ChargingBenchBlockEntity)) return null;

        return new ChargingBenchMenu(inventory, (ChargingBenchBlockEntity) tile, windowId);
    };

    private final Inventory playerInventory;
    private final ChargingBenchBlockEntity tile;

    protected int powerStored = 0;

    public ChargingBenchMenu(Inventory inventory, ChargingBenchBlockEntity tile, int windowId) {
        super(ExpansionMenus.CHARGING_BENCH_MENU.get(), windowId, tile);
        this.playerInventory = inventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(inventory, 8, 101, this::addSlot);
        addChargingBenchInventory();

        addDataSlot(new SimpleDataSlot(tile::getPowerStored, value -> powerStored = value));
    }

    private void addChargingBenchInventory() {
        InventoryLib.addInventory(tile.getInventory(), 0, 88, 17, 4, 2, ChargeableItemSlot::new, this::addSlot);
        InventoryLib.addInventory(tile.getInventory(), 8, 88, 57, 4, 2, ChargeableItemSlot::new, this::addSlot);
    }

    @Override
    public boolean stillValid(Player player) {
        //TODO move to superclass once reobf bug is fixed
        return !tile.isRemoved();
    }

    @Override
    public ItemStack quickMoveStack(Player player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (!slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isChargingStorage(slotIndex) || isFullChargeStorage(slotIndex)) {
            if (!moveToEntireInventory(stack, true)) return ItemStack.EMPTY;
        } else if (stack.getItem() instanceof IChargable) {
            if (!moveToChargingStorage(stack, false)) return ItemStack.EMPTY;
        } else if (isPlayerInventory(slotIndex)) {
            if (!moveToHotbar(stack, false)) return ItemStack.EMPTY;
        } else if (isHotbar(slotIndex)) {
            if (!moveToPlayerInventory(stack, false)) return ItemStack.EMPTY;
        }

        if (stack.isEmpty()) {
            slot.set(ItemStack.EMPTY);
        } else {
            slot.setChanged();
        }

        if (stack.getCount() == originalStack.getCount()) {
            return ItemStack.EMPTY;
        }

        slot.onTake(player, stack);
        return originalStack;
    }

    public int getPowerStoredScaled(int scale) {
        return Math.min(scale, scale * powerStored / tile.getMaxStorage());
    }

    public boolean isPowerStorageFull() {
        return powerStored == tile.getMaxStorage();
    }

    public boolean isStorageCharging() {
        return condCharge > tile.getConductorUpperChargeTarget() && powerStored < tile.getMaxStorage();
    }

    public boolean areItemsCharging() {
        if (powerStored == 0) return false;

        for (int i = 0; i < 8; i++) {
            ItemStack stack = tile.getInventory().getItem(i);
            if (stack.getItem() instanceof IChargable) {
                if (!((IChargable) stack.getItem()).isFullyCharged(stack)) {
                    return true;
                }
            }
        }
        return false;
    }

    //@formatter:off
    private boolean isPlayerInventory(int slotIndex) {
        return slotIndex >= 0 && slotIndex < 27;
    }
    private boolean isHotbar(int slotIndex) {
        return slotIndex >= 27 && slotIndex < 36;
    }
    private boolean isChargingStorage(int slotIndex) {
        return slotIndex >= 36 && slotIndex < 44;
    }
    private boolean isFullChargeStorage(int slotIndex) {
        return slotIndex >= 44 && slotIndex < 52;
    }

    private boolean moveToPlayerInventory(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 0, 27, reverse);
    }
    private boolean moveToHotbar(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 27, 36, reverse);
    }
    private boolean moveToEntireInventory(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 0, 36, reverse);
    }
    private boolean moveToChargingStorage(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 36, 44, reverse);
    }
    private boolean moveToFullStorage(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 44, 52, reverse);
    }
    //@formatter:on

    private static class ChargeableItemSlot extends Slot {

        public ChargeableItemSlot(Container inventory, int slot, int x, int y) {
            super(inventory, slot, x, y);
        }

        @Override
        public boolean mayPlace(ItemStack stack) {
            return stack.getItem() instanceof IChargable;
        }
    }

}
