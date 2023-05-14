package mrtjp.projectred.expansion.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.core.inventory.container.BasePoweredTileContainer;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import mrtjp.projectred.expansion.item.IChargable;
import mrtjp.projectred.expansion.item.IRechargableBattery;
import mrtjp.projectred.expansion.tile.BatteryBoxTile;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;

public class BatteryBoxContainer extends BasePoweredTileContainer {

    public static ICCLContainerFactory<BatteryBoxContainer> FACTORY = (windowId, inventory, packet) -> {
        TileEntity tile = inventory.player.level.getBlockEntity(packet.readPos());
        if (!(tile instanceof BatteryBoxTile)) return null;

        return new BatteryBoxContainer(inventory, (BatteryBoxTile) tile, windowId);
    };

    private final PlayerInventory playerInventory;
    private final BatteryBoxTile tile;

    protected int powerStored = 0;

    public BatteryBoxContainer(PlayerInventory inventory, BatteryBoxTile tile, int windowId) {
        super(ExpansionReferences.BATTERY_BOX_CONTAINER, windowId, tile);
        this.playerInventory = inventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(inventory, 8, 89, this::addSlot);
        addBatteryBoxInventory();
    }

    private void addBatteryBoxInventory() {
        addSlot(new BatterySlot(tile.getInventory(), 0, 80, 31)); // charge slot
        addSlot(new BatterySlot(tile.getInventory(), 1, 80, 53)); // discharge slot
    }

    @Override
    public void broadcastChanges() { //TODO switch to Minecraft's Data Slot system
        super.broadcastChanges();

        boolean needsPower = tile.getPowerStored() != powerStored;
        powerStored = tile.getPowerStored();

        if (needsPower) {
            for (IContainerListener listener : containerListeners) {
                listener.setContainerData(this, 300, powerStored);
            }
        }
    }

    @Override
    public void setData(int id, int value) { //TODO switch to Minecraft's Data Slot system
        switch (id) {
            case 300:
                powerStored = value;
                break;
            default:
                super.setData(id, value);
        }
    }

    @Override
    public ItemStack quickMoveStack(PlayerEntity player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (slot == null || !slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isStorage(slotIndex)) {
            if (!moveToEntireInventory(stack, true)) return ItemStack.EMPTY;
        } else if (stack.getItem() instanceof IChargable) {

            boolean itemHasPower = ((IChargable) stack.getItem()).getStoredPower(stack) > 0;
            boolean tileHasSpace = tile.getPowerStored() < tile.getMaxStorage();

            if (!moveToStorage(stack, itemHasPower && tileHasSpace)) return ItemStack.EMPTY; // to discharge slot if discharge possible, else charge slot
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

    public boolean isStorageDischarging() {
        return condCharge < tile.getConductorLowerChargeTarget() && powerStored > 0;
    }

    //@formatter:off
    private boolean isPlayerInventory(int slotIndex) {
        return slotIndex >= 0 && slotIndex < 27;
    }
    private boolean isHotbar(int slotIndex) {
        return slotIndex >= 27 && slotIndex < 36;
    }
    private boolean isStorage(int slotIndex) {
        return slotIndex >= 36 && slotIndex < 38;
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
    private boolean moveToStorage(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 36, 38, reverse);
    }
    //@formatter:on

    private static class BatterySlot extends Slot {

        public BatterySlot(IInventory inventory, int slot, int x, int y) {
            super(inventory, slot, x, y);
        }

        @Override
        public boolean mayPlace(ItemStack stack) {
            return stack.getItem() instanceof IRechargableBattery;
        }
    }
}
