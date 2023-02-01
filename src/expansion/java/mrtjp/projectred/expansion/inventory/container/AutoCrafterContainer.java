package mrtjp.projectred.expansion.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.expansion.tile.AutoCrafterTile;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;

public class AutoCrafterContainer extends BaseMachineContainer {

    public static final ICCLContainerFactory<AutoCrafterContainer> FACTORY = (windowId, playerInv, packet) -> {
        TileEntity tile = playerInv.player.level.getBlockEntity(packet.readPos());
        if (!(tile instanceof AutoCrafterTile)) return null;
        return new AutoCrafterContainer(playerInv, (AutoCrafterTile) tile, windowId);
    };

    private final PlayerInventory playerInventory;
    private final AutoCrafterTile tile;

    private int planSlot;

    public AutoCrafterContainer(PlayerInventory inventory, AutoCrafterTile tile, int windowId) {
        super(ExpansionReferences.AUTO_CRAFTER_CONTAINER, windowId, tile);
        this.playerInventory = inventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(inventory, 8, 130, this::addSlot);
        addAutoCrafterInventory();
    }

    private void addAutoCrafterInventory() {

        // Storage slot
        InventoryLib.addInventory(tile.getStorageInventory(), 0, 8, 80, 9, 2, this::addSlot);

        // Plan grid
        InventoryLib.addInventory(tile.getPlanInventory(), 0, 44, 22, 3, 3, PlanSlot::new, this::addSlot);
    }

    public AutoCrafterTile getAutoCrafterTile() {
        return tile;
    }

    public ItemStack getPlanOutput() {
        ItemStack planStack = tile.getPlanInventory().getItem(planSlot);
        return RecipePlanItem.loadPlanOutput(planStack);
    }

    public int getPlanSlot() {
        return planSlot;
    }

    @Override
    public void broadcastChanges() {
        super.broadcastChanges();

        if (planSlot != tile.getPlanSlot()) {
            planSlot = tile.getPlanSlot();
            for (IContainerListener listener : containerListeners) {
                listener.setContainerData(this, 200, planSlot);
            }
        }
    }

    @Override
    public void setData(int id, int value) {
        switch (id) {
            case 200:
                planSlot = value;
                break;
            default:
                super.setData(id, value);
        }
    }

    //region Quickmove
    @Override
    public ItemStack quickMoveStack(PlayerEntity player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (slot == null || !slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isPlanSlots(slotIndex)) {
            if (!moveToEntireInventory(stack, false) && !moveToStorage(stack, false)) return ItemStack.EMPTY;

        } else if (isStorage(slotIndex)) {
            if (stack.getItem() instanceof RecipePlanItem) {
                if (!moveToPlanSlots(stack, false) && !moveToEntireInventory(stack, false)) return ItemStack.EMPTY;
            } else {
                if (!moveToEntireInventory(stack, false)) return ItemStack.EMPTY;
            }

        } else if (isPlayerInventory(slotIndex) || isHotbar(slotIndex)) {
            if (stack.getItem() instanceof RecipePlanItem) {
                if (!moveToPlanSlots(stack, false) && !moveToStorage(stack, false)) return ItemStack.EMPTY;
            } else if (isPlayerInventory(slotIndex)) {
                if (!moveToStorage(stack, false) && !moveToHotbar(stack, false)) return ItemStack.EMPTY;
            } else {
                if (!moveToStorage(stack, false) && !moveToPlayerInventory(stack, false)) return ItemStack.EMPTY;
            }
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

    //@formatter:off
    private boolean isPlayerInventory(int slotIndex) {
        return slotIndex >= 0 && slotIndex < 27;
    }
    private boolean isHotbar(int slotIndex) {
        return slotIndex >= 27 && slotIndex < 36;
    }
    private boolean isStorage(int slotIndex) {
        return slotIndex >= 36 && slotIndex < 54;
    }
    private boolean isPlanSlots(int slotIndex) {
        return slotIndex >= 54 && slotIndex < 63;
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
        return moveItemStackTo(stack, 36, 54, reverse);
    }
    private boolean moveToPlanSlots(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 54, 63, reverse);
    }
    //@formatter:on

    private static class PlanSlot extends Slot {
        public PlanSlot(IInventory inventory, int index, int x, int y) {
            super(inventory, index, x, y);
        }

        @Override
        public boolean mayPlace(ItemStack stack) {
            return RecipePlanItem.hasRecipeInside(stack);
        }

        @Override
        public int getMaxStackSize() {
            return 1;
        }
    }
}
