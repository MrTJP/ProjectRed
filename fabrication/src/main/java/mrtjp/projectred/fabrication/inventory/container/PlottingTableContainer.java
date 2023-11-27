package mrtjp.projectred.fabrication.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.core.inventory.container.TakeOnlySlot;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.fabrication.item.BlankPhotomaskItem;
import mrtjp.projectred.fabrication.item.ICBlueprintItem;
import mrtjp.projectred.fabrication.tile.PlottingTableTile;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntity;

public class PlottingTableContainer extends FabricationMachineContainer {

    public static ICCLContainerFactory<PlottingTableContainer> FACTORY = (windowId, inventory, packet) -> {
        BlockEntity tile = inventory.player.level.getBlockEntity(packet.readPos());
        if (!(tile instanceof PlottingTableTile)) return null;

        return new PlottingTableContainer(inventory, (PlottingTableTile) tile, windowId);
    };

    private final Inventory playerInventory;
    private final PlottingTableTile tile;

    public PlottingTableContainer(Inventory playerInventory, PlottingTableTile tile, int windowId) {
        super(FabricationReferences.PLOTTING_TABLE_CONTAINER, windowId, tile);
        this.playerInventory = playerInventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(playerInventory, 8, 89, this::addSlot);
        addPlottingTableInventory();
    }

    private void addPlottingTableInventory() {
        addSlot(new Slot(tile.getInventory(), 0, 56, 31)); // blueprint input
        addSlot(new Slot(tile.getInventory(), 1, 56, 49)); // empty photo mask input
        addSlot(new TakeOnlySlot(tile.getInventory(), 2, 116, 40)); // output
    }

    @Override
    public ItemStack quickMoveStack(Player player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (slot == null || !slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isOutput(slotIndex)) {
            if (!moveToEntireInventory(stack, true)) return ItemStack.EMPTY;

        } else if (isHotbar(slotIndex) || isPlayerInventory(slotIndex)) {
            if (isBlueprint(stack)) {
                if (!moveToBlueprintInput(stack, false)) return ItemStack.EMPTY;

            } else if (isBlankPhotomask(stack)) {
                if (!moveToPhotomaskInput(stack, false)) return ItemStack.EMPTY;

            } else if (isPlayerInventory(slotIndex)) {
                if (!moveToHotbar(stack, false)) return ItemStack.EMPTY;

            } else { // Hot bar
                if (!moveToPlayerInventory(stack, false)) return ItemStack.EMPTY; // Move to player inventory
            }

        } else if (isInputs(slotIndex)) {
            if (!moveToEntireInventory(stack, false)) return ItemStack.EMPTY;
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
    private boolean isHotbar(int slotIndex) {
        return slotIndex >= 27 && slotIndex < 36;
    }
    private boolean isPlayerInventory(int slotIndex) {
        return slotIndex >= 0 && slotIndex < 27;
    }
    private boolean isInputs(int slotIndex) {
        return slotIndex >= 36 && slotIndex < 38;
    }
    private boolean isOutput(int slotIndex) {
        return slotIndex == 38;
    }

    private boolean moveToHotbar(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 27, 36, reverse);
    }
    private boolean moveToPlayerInventory(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 0, 27, reverse);
    }
    private boolean moveToEntireInventory(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 0, 36, reverse);
    }
    private boolean moveToBlueprintInput(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 36, 37, reverse);
    }
    private boolean moveToPhotomaskInput(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 37, 38, reverse);
    }
    //@formatter:on

    private boolean isBlueprint(ItemStack stack) {
        return stack.getItem() instanceof ICBlueprintItem; //TODO check tag
    }

    private boolean isBlankPhotomask(ItemStack stack) {
        return stack.getItem() instanceof BlankPhotomaskItem;
    }
}
