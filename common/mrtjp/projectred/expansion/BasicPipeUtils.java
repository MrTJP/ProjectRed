package mrtjp.projectred.expansion;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.ISidedInventory;
import net.minecraft.inventory.InventoryLargeChest;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntityChest;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;

public class BasicPipeUtils {

    public static boolean addToInventory(World world, ItemStack item, BlockCoord bc, int side) {
        return tryAddToInventory(world, item, bc, side, true);
    }

    public static boolean canAddToInventory(World world, ItemStack item, BlockCoord bc, int side) {
        return tryAddToInventory(world, item, bc, side, false);
    }

    public static boolean tryAddToInventory(World world, ItemStack item, BlockCoord bc, int side, boolean doAdd) {
        IInventory inv = getInventory(world, bc);
        if (inv == null)
            return false;

        int[] slots = new int[inv.getSizeInventory()];

        if (inv instanceof ISidedInventory)
            slots = ((ISidedInventory)inv).getAccessibleSlotsFromSide(side);
        else
            for (int i = 0; i < inv.getSizeInventory(); i++)
                slots[i] = i;

        return addIntoInventory(inv, item, slots, doAdd);
    }

    public static boolean addIntoInventory(IInventory inv, ItemStack item, int[] slotsToCheck, boolean doAdd) {
        for (int n : slotsToCheck) {
            ItemStack stackInSlot = inv.getStackInSlot(n);
            if (stackInSlot == null) {
                if (!doAdd)
                    return true;

            } else if (item.isItemEqual(stackInSlot) && ItemStack.areItemStackTagsEqual(item, stackInSlot)) {
                int freeSpace = Math.min(stackInSlot.getMaxStackSize(), inv.getInventoryStackLimit());

                freeSpace -= stackInSlot.stackSize;
                if (freeSpace > 0) {
                    int toAdd = Math.min(freeSpace, item.stackSize);
                    if (!doAdd)
                        return true;

                    stackInSlot.stackSize += toAdd;
                    inv.setInventorySlotContents(n, stackInSlot);
                    item.stackSize -= toAdd;
                    if (item.stackSize == 0)
                        return true;
                }
            }
        }
        if (!doAdd)
            return false;
        for (int n : slotsToCheck) {
            ItemStack invst = inv.getStackInSlot(n);
            if (invst == null) {
                if (inv.getInventoryStackLimit() >= item.stackSize) {
                    inv.setInventorySlotContents(n, item);
                    return true;
                }
                inv.setInventorySlotContents(n, item.splitStack(inv.getInventoryStackLimit()));
            }
        }

        return false;
    }

    public static IInventory getInventory(World world, BlockCoord wc) {
        IInventory inv = BasicUtils.getTileEntity(world, wc, IInventory.class);

        if (!(inv instanceof TileEntityChest))
            return inv;

        for (int i = 2; i < 6; i++) {
            TileEntityChest chest = BasicUtils.getTileEntity(world, wc.copy().offset(i), TileEntityChest.class);
            if (chest != null)
                return new InventoryLargeChest("Large chest", chest, inv);
        }
        return inv;
    }

}
