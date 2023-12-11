package mrtjp.projectred.fabrication.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.core.inventory.container.SimpleDataSlot;
import mrtjp.projectred.core.inventory.container.TakeOnlySlot;
import mrtjp.projectred.fabrication.init.FabricationMenus;
import mrtjp.projectred.fabrication.item.ValidDieItem;
import mrtjp.projectred.fabrication.tile.PackagingTableTile;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntity;

public class PackagingTableContainer extends FabricationMachineContainer {

    public static ICCLContainerFactory<PackagingTableContainer> FACTORY = (windowId, inventory, packet) -> {
        BlockEntity tile = inventory.player.level.getBlockEntity(packet.readPos());
        if (!(tile instanceof PackagingTableTile)) return null;

        return new PackagingTableContainer(inventory, (PackagingTableTile) tile, windowId);
    };

    private final Inventory playerInventory;
    private final PackagingTableTile tile;

    protected int problematicSlotMask;

    public PackagingTableContainer(Inventory playerInventory, PackagingTableTile tile, int windowId) {
        super(FabricationMenus.PACKAGING_TABLE_CONTAINER.get(), windowId, tile);
        this.playerInventory = playerInventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(playerInventory, 8, 89, this::addSlot);
        addPackagingTableInventory();

        addDataSlot(new SimpleDataSlot(tile::getProblematicSlotMask, value -> problematicSlotMask = value));
    }

    private void addPackagingTableInventory() {
        // Input slots
        InventoryLib.addInventory(tile.getInventory(), 0, 46, 22, 3, 3, this::addSlot);

        // Output slot
        addSlot(new TakeOnlySlot(tile.getInventory(), 9, 135, 40));
    }

    @Override
    public ItemStack quickMoveStack(Player player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (!slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isOutput(slotIndex)) {
            if (!moveToEntireInventory(stack, true)) return ItemStack.EMPTY;

        } else if (isHotbar(slotIndex) || isPlayerInventory(slotIndex)) {
            if (isValidDie(stack)) {
                if (!moveToDieInput(stack, false)) return ItemStack.EMPTY;

            } else if (isPlayerInventory(slotIndex)) {
                if (!moveToHotbar(stack, false)) return ItemStack.EMPTY;

            } else { // Hot bar
                if (!moveToPlayerInventory(stack, false)) return ItemStack.EMPTY;
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

    public int getProblematicSlotMask() {
        return problematicSlotMask;
    }

    //@formatter:off
    private boolean isHotbar(int slotIndex) {
        return slotIndex >= 27 && slotIndex < 36;
    }
    private boolean isPlayerInventory(int slotIndex) {
        return slotIndex >= 0 && slotIndex < 27;
    }
    private boolean isInputs(int slotIndex) {
        return slotIndex >= 36 && slotIndex < 45;
    }
    private boolean isOutput(int slotIndex) {
        return slotIndex == 45;
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
    private boolean moveToDieInput(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 40, 41, reverse);
    }
    //@formatter:on

    private boolean isValidDie(ItemStack stack) {
        return stack.getItem() instanceof ValidDieItem; //TODO check tag
    }
}
