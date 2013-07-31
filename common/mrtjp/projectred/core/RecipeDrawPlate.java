package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.multipart.microblocks.ItemBlockMicroblock;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.world.World;

public class RecipeDrawPlate implements IRecipe {

	@Override
	public boolean matches(InventoryCrafting inv, World world) {
		return getCraftingResult(inv) != null;
	}

	@Override
	public ItemStack getCraftingResult(InventoryCrafting inv) {
		for (int i = 0; i < inv.getSizeInventory(); i++) {
			ItemStack stack = inv.getStackInSlot(i);
			if (i == 0 || i == 2 || i == 6 || i == 8) {
				if (stack != null) {
					return null;
				}
			} else if (i == 1 || i == 3 || i == 5 || i == 8) {
				if (stack == null) {
					return null;
				}
				if (ItemBlockMicroblock.getPartTypeID(stack) != 3145) {
					return null;
				}
			} else if (i == 4) {
				if (stack == null) {
					return null;
				}
				if (ItemBlockMicroblock.getPartTypeID(stack) != 3649) {
					return null;
				}
			}
		}
		
		return getRecipeOutput();
	}

	@Override
	public int getRecipeSize() {
		return 3;
	}

	@Override
	public ItemStack getRecipeOutput() {
		return new ItemStack(ProjectRed.itemDrawPlate, 1);
	}

}
