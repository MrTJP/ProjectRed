package mrtjp.projectred.exploration.inventory.container;

import codechicken.lib.inventory.container.CCLMenuType;
import mrtjp.projectred.exploration.init.ExplorationMenus;
import mrtjp.projectred.exploration.inventory.BackpackInventory;
import mrtjp.projectred.exploration.item.BackpackItem;
import mrtjp.projectred.exploration.item.component.BackpackDataComponent;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ClickType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;


public class BackpackMenu extends AbstractContainerMenu {

    public static final CCLMenuType<BackpackMenu> FACTORY = (windowId, playerInv, packet) -> new BackpackMenu(windowId, playerInv);

    private final BackpackInventory inventory;// = new BackpackInventory(27);

    private final Inventory playerInventory;

    public BackpackMenu(int windowId, Inventory playerInventory) {
        super(ExplorationMenus.BACKPACK_MENU.get(), windowId);

        this.playerInventory = playerInventory;
        this.inventory = this.loadInventoryFromMainHand(playerInventory);

        inventory.addListener(this::onBackpackInventoryChanged);

        InventoryLib.addPlayerInventory(playerInventory, 8, 86, BackpackPlayerSlot::new, this::addSlot);
        InventoryLib.addInventory(inventory, 0, 8, 18, 9, 3, BackpackSlot::new, this::addSlot);
    }

    public BackpackInventory loadInventoryFromMainHand(Inventory playerInventory) {
        ItemStack backpack = playerInventory.player.getMainHandItem();
        if (!BackpackItem.isBackpack(backpack)) {
            return new BackpackInventory(27);
        }

        // Get or create the backpack data component
        var component = BackpackDataComponent.getOrCreateComponent(backpack, 27, true);

        // Save it in opened state. Previously, we also emptied it here to prevent duplication.
        BackpackDataComponent.setComponent(backpack, component);

        return component.createInventory();
    }

    public void saveInventoryToMainHand(Inventory playerInventory) {
        ItemStack backpack = playerInventory.player.getMainHandItem();
        if (!BackpackItem.isBackpack(backpack)) {
            return;
        }

        // Convert inventory to a Backpack component with closed state
        var component = BackpackDataComponent.fromInventory(inventory, false);

        // Save it
        BackpackDataComponent.setComponent(backpack, component);
    }

    @Override
    public void clicked(int slotId, int dragType, ClickType clickType, Player player) {
        // Required because vanilla's shyte handling of number hotbar quick swap.
        // Apparently it only asks the non-hotbar slot if it can accept the item, but it does not ask the
        // hotbar slot (corresponding to number pressed) if it can take the stack (which would be denied by the custom slot class's canTakeStack implemented above).
        // Additionally, if the non-hotbar slot is empty, it calls that empty slot's canTakeStack. This is almost certainly
        // a bug. canTakeStack should be called on BOTH slots, and isItemValid should be called on BOTH slots with the other slots contents.
        //
        // Update 1.18.2: Still, only the non-hotbar slot is checked for mayPickup/mayPlace results. Empty non-hotbar slot is no longer asked for
        //                canTakeStack (instead its correctly asked mayPlace) but the hotbar slot is still not asked for canTakeStack.

        if (isHotbar(slotId)) {
            int hotbarIndex = slotId - 27;
            if (hotbarIndex == player.getInventory().selected) {
                return;
            }
        }

        if (clickType == ClickType.SWAP) {
            if (dragType == player.getInventory().selected) {
                return;
            }
        }

        super.clicked(slotId, dragType, clickType, player);
    }

    @Override
    public void removed(Player player) {
        super.removed(player);
        saveInventoryToMainHand(playerInventory);
    }

    @Override
    public boolean stillValid(Player player) {
        ItemStack stack = player.getMainHandItem();
        return BackpackItem.isBackpack(stack);
    }

    protected void onBackpackInventoryChanged(Container backpackInventory) {
        // Note: no need to save here. Inventory is saved on container close
    }

    @Override
    public ItemStack quickMoveStack(Player player, int slotIndex) {
        Slot slot = slots.get(slotIndex);
        if (!slot.hasItem()) return ItemStack.EMPTY;

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
        public BackpackPlayerSlot(Container inventory, int slotIndex, int x, int y) {
            super(inventory, slotIndex, x, y);
        }
        @Override
        public boolean mayPickup(Player player) {
            // Dont allow pickup if this is the slot containing the selected backpack
            return getSlotIndex() != player.getInventory().selected;
        }
    }

    /**
     * Slot for the backpack contents inventory. Enforces the backpack blacklist.
     */
    private static class BackpackSlot extends Slot {
        public BackpackSlot(Container inventory, int slotIndex, int x, int y) {
            super(inventory, slotIndex, x, y);
        }

        @Override
        public boolean mayPlace(ItemStack stack) {
            return BackpackItem.isItemAllowedInBackpack(stack);
        }
    }
}
