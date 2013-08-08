package mrtjp.projectred.expansion;

import mrtjp.projectred.ProjectRed;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.world.World;

public class RecipeVAWTRecoloring implements IRecipe {

    @Override
    public boolean matches(InventoryCrafting inv, World world) {
        return getCraftingResult(inv) != null;
    }

    @Override
    public ItemStack getCraftingResult(InventoryCrafting inv) {
        if (inv.getStackInSlot(4) == null || inv.getStackInSlot(4).itemID != ProjectRed.itemVAWT.itemID) {
            return null;
        }
        if (inv.getStackInSlot(3) != null ||  inv.getStackInSlot(5) != null) {
            return null;
        }
        ItemStack stack = inv.getStackInSlot(4);
        ItemVAWT.initNBT(stack);
        int[] colors = stack.getTagCompound().getIntArray("colors");
        ItemStack dye0 = inv.getStackInSlot(0) != null ? inv.getStackInSlot(0).itemID == Item.dyePowder.itemID ? inv.getStackInSlot(0) : null : null;
        ItemStack dye1 = inv.getStackInSlot(1) != null ? inv.getStackInSlot(1).itemID == Item.dyePowder.itemID ? inv.getStackInSlot(1) : null : null;
        ItemStack dye2 = inv.getStackInSlot(2) != null ? inv.getStackInSlot(2).itemID == Item.dyePowder.itemID ? inv.getStackInSlot(2) : null : null;
        ItemStack dye3 = inv.getStackInSlot(6) != null ? inv.getStackInSlot(6).itemID == Item.dyePowder.itemID ? inv.getStackInSlot(6) : null : null;
        ItemStack dye4 = inv.getStackInSlot(7) != null ? inv.getStackInSlot(7).itemID == Item.dyePowder.itemID ? inv.getStackInSlot(7) : null : null;
        ItemStack dye5 = inv.getStackInSlot(8) != null ? inv.getStackInSlot(8).itemID == Item.dyePowder.itemID ? inv.getStackInSlot(8) : null : null;
        colors[0] =  dye0 != null ? 15 - dye0.getItemDamage() : colors[0];
        colors[1] =  dye1 != null ? 15 - dye1.getItemDamage() : colors[1];
        colors[2] =  dye2 != null ? 15 - dye2.getItemDamage() : colors[2];
        colors[3] =  dye3 != null ? 15 - dye3.getItemDamage() : colors[3];
        colors[4] =  dye4 != null ? 15 - dye4.getItemDamage() : colors[4];
        colors[5] =  dye5 != null ? 15 - dye5.getItemDamage() : colors[5];
        NBTTagCompound nbt = new NBTTagCompound();
        nbt.setIntArray("colors", colors);
        stack.setTagCompound(nbt);
        return stack;
    }

    @Override
    public int getRecipeSize() {
        return 3;
    }

    @Override
    public ItemStack getRecipeOutput() {
        return new ItemStack(ProjectRed.itemVAWT, 1, 0);
    }

}
