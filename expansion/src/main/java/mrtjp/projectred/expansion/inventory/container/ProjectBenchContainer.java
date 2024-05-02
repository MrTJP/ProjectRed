package mrtjp.projectred.expansion.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.expansion.init.ExpansionMenus;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.expansion.tile.ProjectBenchTile;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ResultSlot;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntity;

public class ProjectBenchContainer extends AbstractContainerMenu {

    public static final ICCLContainerFactory<ProjectBenchContainer> FACTORY = (windowId, playerInv, packet) -> {
        BlockEntity tile = playerInv.player.level().getBlockEntity(packet.readPos());
        if (!(tile instanceof ProjectBenchTile)) return null;
        return new ProjectBenchContainer(playerInv, (ProjectBenchTile) tile, windowId);
    };

    private final ProjectBenchTile tile;
    private final Inventory playerInventory;

    public ProjectBenchContainer(Inventory playerInventory, ProjectBenchTile tile, int windowId) {
        super(ExpansionMenus.PROJECT_BENCH_CONTAINER.get(), windowId);
        this.playerInventory = playerInventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(playerInventory, 8, 126, this::addSlot);
        addProjectBenchInventory();
    }

    private void addProjectBenchInventory() {

        // Storage slots
        InventoryLib.addInventory(tile.getStorageInventory(), 0, 8, 76, 9, 2, this::addSlot);

        // Crafting grid
        InventoryLib.addInventory(tile.getCraftingGridInventory(), 0, 48, 18, 3, 3, this::addSlot);

        // Result slot
        addSlot(new ProjectBenchCraftingSlot(143, 36));

        // Plan slot
        addSlot(new Slot(tile.getPlanInventory(), 0, 17, 36) {
            @Override
            public boolean mayPlace(ItemStack stack) {
                return stack.getItem() instanceof RecipePlanItem;
            }

            @Override
            public int getMaxStackSize() {
                return 1;
            }
        });
    }

    public ProjectBenchTile getProjectBenchTile() {
        return tile;
    }

    @Override
    public boolean stillValid(Player player) {
        return !tile.isRemoved(); //TODO
    }

    //region Utils
    protected void addPlayerInventory(Inventory playerInventory, int x, int y) {
        addInventory(playerInventory, 9, x, y, 9, 3); // Inventory (0 - 26)
        addInventory(playerInventory, 0, x, y + 58, 9, 1); // Hotbar slots (27 - 35)
    }

    protected void addInventory(Container inventory, int i, int x, int y, int columns, int rows) {
        for (int c = 0; c < columns; c++) {
            for (int r = 0; r < rows; r++) {
                addSlot(new Slot(inventory, i + (r * columns + c), x + c * 18, y + r * 18));
            }
        }
    }

    public void transferAllFromGrid() {
        for (int i = 0; i < 9; i++) {
            int craftingGridSlot = i + 54;
            if (getSlot(craftingGridSlot).hasItem())
                quickMoveStack(playerInventory.player, craftingGridSlot);
        }
        broadcastChanges();
    }
    //endregion

    //region Quickmove
    @Override
    public ItemStack quickMoveStack(Player player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (!slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isCraftingGrid(slotIndex)) {
            if (!moveToStorage(stack, false) && !moveToEntireInventory(stack, false)) return ItemStack.EMPTY;

        } else if (isStorage(slotIndex)) {
            if (stack.getItem() instanceof RecipePlanItem) {
                // Transfer out existing plan if exists
                if (!getSlot(64).getItem().isEmpty()) quickMoveStack(player, 64);
                if (!moveToPlanSlot(stack, false) && !moveToEntireInventory(stack, false)) return ItemStack.EMPTY;
            } else {
                if (!moveToEntireInventory(stack, false)) return ItemStack.EMPTY;
            }

        } else if (isPlanSlot(slotIndex)) {
            if (!moveToStorage(stack, true) && !moveToEntireInventory(stack, false)) return ItemStack.EMPTY;

        } else if (isResultSlot(slotIndex)) {
            // Usualy, mayPickup is only queried once, and then this method is called repeatedly until EMPTY is returned.
            // Since this slot is allowed to remain non-empty even after pickup is not possible, we must check this every time.
            // See AbstractContainerMenu#doClick
            if (!slot.mayPickup(player)) return ItemStack.EMPTY;
            if (!moveToEntireInventory(stack, true) && !moveToStorage(stack, true)) return ItemStack.EMPTY;

        } else if (isPlayerInventory(slotIndex) || isHotbar(slotIndex)) {
            if (stack.getItem() instanceof RecipePlanItem) {
                // Transfer out existing plan if exists
                if (!getSlot(64).getItem().isEmpty()) quickMoveStack(player, 64);
                if (!moveToPlanSlot(stack, false) && !moveToStorage(stack, false)) return ItemStack.EMPTY;
            } else if (isPlayerInventory(slotIndex)) {
                if (!moveToStorage(stack, false) && !(moveToHotbar(stack, false))) return ItemStack.EMPTY;
            } else {
                if (!moveToPlayerInventory(stack, false)) return ItemStack.EMPTY;
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

    @Override
    public boolean canTakeItemForPickAll(ItemStack stack, Slot slot) {
        return slot.container != tile.getCraftingHelper().getCraftResultInventory();
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
    private boolean isCraftingGrid(int slotIndex) {
        return slotIndex >= 54 && slotIndex < 63;
    }
    private boolean isResultSlot(int slotIndex) {
        return slotIndex == 63;
    }
    private boolean isPlanSlot(int slotIndex) {
        return slotIndex == 64;
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
    private boolean moveToCraftingGrid(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 54, 63, reverse);
    }
    private boolean moveToPlanSlot(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 64, 65, reverse);
    }
    //@formatter:on

    //endregion

    private class ProjectBenchCraftingSlot extends ResultSlot {

        public ProjectBenchCraftingSlot(int x, int y) {
            super(playerInventory.player, tile.getCraftingHelper().getCraftingInventory(),
                    tile.getCraftingHelper().getCraftResultInventory(), 0, x, y);
        }

        @Override
        public boolean mayPickup(Player player) {
            return tile.getCraftingHelper().canTake();
        }

        @Override
        public void onTake(Player player, ItemStack stack) {
            checkTakeAchievements(stack);
            tile.getCraftingHelper().onCraftedByPlayer(player, !tile.isPlanRecipe());
            tile.updateRecipeIfNeeded();
        }
    }
}
