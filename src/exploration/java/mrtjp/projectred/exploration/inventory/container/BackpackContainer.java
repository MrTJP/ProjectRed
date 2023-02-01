package mrtjp.projectred.exploration.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.exploration.inventory.BackpackInventory;
import mrtjp.projectred.exploration.item.BackpackItem;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;

import static mrtjp.projectred.exploration.init.ExplorationReferences.BACKPACK_CONTAINER;

public class BackpackContainer extends Container {

    public static final ICCLContainerFactory<BackpackContainer> FACTORY = (windowId, playerInv, packet) -> new BackpackContainer(windowId, playerInv);

    private final BackpackInventory inventory = new BackpackInventory(27);

    private final PlayerInventory playerInventory;

    public BackpackContainer(int windowId, PlayerInventory playerInventory) {
        super(BACKPACK_CONTAINER, windowId);

        this.playerInventory = playerInventory;

        InventoryLib.addPlayerInventory(playerInventory, 8, 86, BackpackPlayerSlot::new, this::addSlot);
        InventoryLib.addInventory(inventory, 0, 8, 18, 9, 3, BackpackSlot::new, this::addSlot);

        inventory.addListener(this::onBackpackInventoryChanged);
        inventory.loadInventoryFromMainHand(playerInventory);
    }

    @Override
    public ItemStack clicked(int slotId, int dragType, ClickType clickType, PlayerEntity player) {
        // Required because vanilla's shyte handling of number hotbar quick swap.
        // Apparently it only asks the target slot (the one hovered over) if it can accept the item, but it does not ask the
        // hotbar slot (corresponding to number pressed) if it can take the stack (which would be denied by the custom slot class's canTakeStack implemented above).
        // Additionally, if the target slot is empty, it calls that empty slot's canTakeStack. This is almost certainly
        // a bug. canTakeStack should be called on BOTH slots, and isItemValid should be called on BOTH slots with the other slots contents.

        if (isHotbar(slotId)) {
            int hotbarIndex = slotId - 27;
            if (hotbarIndex == player.inventory.selected) {
                return ItemStack.EMPTY;
            }
        }

        if (clickType == ClickType.SWAP) {
            if (dragType == player.inventory.selected) {
                return ItemStack.EMPTY;
            }
        }

        return super.clicked(slotId, dragType, clickType, player);
    }

    @Override
    public void removed(PlayerEntity player) {
        super.removed(player);
        inventory.saveInventoryToMainHand(playerInventory);
    }

    @Override
    public boolean stillValid(PlayerEntity player) {
        ItemStack stack = player.getMainHandItem();
        return BackpackItem.isBackpack(stack);
    }

    protected void onBackpackInventoryChanged(IInventory backpackInventory) {
        // Note: no need to save here. Inventory is saved on container close
    }

    @Override
    public ItemStack quickMoveStack(PlayerEntity player, int slotIndex) {
        Slot slot = slots.get(slotIndex);
        if (slot == null || !slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isBag(slotIndex)) {
            if (!moveToEntireInventory(stack, true)) return ItemStack.EMPTY;
        } else {
            if (!moveToBag(stack, false)) return ItemStack.EMPTY;
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
    private boolean isBag(int slotIndex) {
        return slotIndex >= 36 && slotIndex < 63;
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
    private boolean moveToBag(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 36, 63, reverse);
    }
    //@formatter:on

    /**
     * Slot for the actual player inventory. Disables picking up backpack from hotbar.
     */
    private static class BackpackPlayerSlot extends Slot {
        public BackpackPlayerSlot(IInventory inventory, int slotIndex, int x, int y) {
            super(inventory, slotIndex, x, y);
        }
        @Override
        public boolean mayPickup(PlayerEntity player) {
            // Dont allow pickup if this is the slot containing the selected backpack
            return getSlotIndex() != player.inventory.selected;
        }
    }

    /**
     * Slot for the backpack contents inventory. Enforces the backpack blacklist.
     */
    private static class BackpackSlot extends Slot {
        public BackpackSlot(IInventory inventory, int slotIndex, int x, int y) {
            super(inventory, slotIndex, x, y);
        }

        @Override
        public boolean mayPlace(ItemStack stack) {
            return BackpackItem.isItemAllowedInBackpack(stack);
        }
    }
}
