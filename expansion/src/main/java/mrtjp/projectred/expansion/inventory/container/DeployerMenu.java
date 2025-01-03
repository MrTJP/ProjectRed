package mrtjp.projectred.expansion.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.expansion.init.ExpansionMenus;
import mrtjp.projectred.expansion.tile.DeployerBlockEntity;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntity;

public class DeployerMenu extends AbstractContainerMenu {

    public static final ICCLContainerFactory<DeployerMenu> FACTORY = (windowId, playerInv, packet) -> {
        BlockEntity tile = playerInv.player.getLevel().getBlockEntity(packet.readPos());
        if (!(tile instanceof DeployerBlockEntity dbe)) return null;
        return new DeployerMenu(playerInv, dbe, windowId);
    };

    private final DeployerBlockEntity tile;
    private final Inventory playerInventory;

    public DeployerMenu(Inventory playerInventory, DeployerBlockEntity tile, int windowId) {
        super(ExpansionMenus.DEPLOYER_MENU.get(), windowId);
        this.playerInventory = playerInventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(playerInventory, 8, 86, this::addSlot);
        addDeployerInventory();
    }

    private void addDeployerInventory() {
        // Storage
        InventoryLib.addInventory(tile.getInventory(), 0, 62, 18, 3, 3, this::addSlot);
    }

    @Override
    public boolean stillValid(Player player) {
        return !tile.isRemoved(); //TODO
    }

    //region Quickmove
    @Override
    public ItemStack quickMoveStack(Player player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (!slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        // Simple chest-like quick-move logic
        if (isStorage(slotIndex)) {
            if (!moveToEntireInventory(stack, true)) return ItemStack.EMPTY;
        } else {
            if (!moveToStorage(stack, false)) return ItemStack.EMPTY;
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
        return slotIndex >= 36 && slotIndex < 45;
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
        return moveItemStackTo(stack, 36, 45, reverse);
    }
    //@formatter:on

    //endregion
}
