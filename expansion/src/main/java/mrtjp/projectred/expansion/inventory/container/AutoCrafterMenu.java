package mrtjp.projectred.expansion.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.core.inventory.container.SimpleDataSlot;
import mrtjp.projectred.expansion.init.ExpansionMenus;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.expansion.tile.AutoCrafterBlockEntity;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntity;

public class AutoCrafterMenu extends BaseMachineMenu {

    public static final ICCLContainerFactory<AutoCrafterMenu> FACTORY = (windowId, playerInv, packet) -> {
        BlockEntity tile = playerInv.player.level().getBlockEntity(packet.readPos());
        if (!(tile instanceof AutoCrafterBlockEntity)) return null;
        return new AutoCrafterMenu(playerInv, (AutoCrafterBlockEntity) tile, windowId);
    };

    private final Inventory playerInventory;
    private final AutoCrafterBlockEntity tile;

    private int planSlot;

    public AutoCrafterMenu(Inventory inventory, AutoCrafterBlockEntity tile, int windowId) {
        super(ExpansionMenus.AUTO_CRAFTER_MENU.get(), windowId, tile);
        this.playerInventory = inventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(inventory, 8, 130, this::addSlot);
        addAutoCrafterInventory();

        addDataSlot(new SimpleDataSlot(tile::getPlanSlot, value -> planSlot = value));
    }

    private void addAutoCrafterInventory() {

        // Storage slot
        InventoryLib.addInventory(tile.getStorageInventory(), 0, 8, 80, 9, 2, this::addSlot);

        // Plan grid
        InventoryLib.addInventory(tile.getPlanInventory(), 0, 44, 22, 3, 3, PlanSlot::new, this::addSlot);
    }

    public AutoCrafterBlockEntity getAutoCrafterTile() {
        return tile;
    }

    public ItemStack getPlanOutput() {
        ItemStack planStack = tile.getPlanInventory().getItem(planSlot);
        return RecipePlanItem.loadPlanOutput(planStack);
    }

    public int getPlanSlot() {
        return planSlot;
    }

    //region Quickmove
    @Override
    public ItemStack quickMoveStack(Player player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (!slot.hasItem()) return ItemStack.EMPTY;

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
        public PlanSlot(Container inventory, int index, int x, int y) {
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
