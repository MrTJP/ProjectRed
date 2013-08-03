package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRed;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.world.World;

public class RecipeBackpackRecoloring implements IRecipe {

	@Override
	public boolean matches(InventoryCrafting inv, World world) {
		return getCraftingResult(inv) != null;
	}

	@Override
	public ItemStack getCraftingResult(InventoryCrafting inv) {
		if (checkIfOnlyContainsBagAndDye(inv)) {
			ItemStack bag = getBagFromInv(inv);
			ItemStack dye = getDyeFromInv(inv);
			ItemStack newBag = new ItemStack(ProjectRed.itemBackpack.itemID, 1, 15 - dye.getItemDamage());
			newBag.setTagCompound(bag.getTagCompound());
			return newBag;
		}
		return null;
	}
	
	private ItemStack getDyeFromInv(InventoryCrafting inv) {
		for (int i = 0; i < inv.getSizeInventory(); i++) {
			ItemStack stack = inv.getStackInSlot(i);
			if (stack != null && stack.itemID == Item.dyePowder.itemID) {
				return stack;
			}
		}
		return null;
	}

	private ItemStack getBagFromInv(InventoryCrafting inv) {
		for (int i = 0; i < inv.getSizeInventory(); i++) {
			ItemStack stack = inv.getStackInSlot(i);
			if (stack != null && stack.itemID == ProjectRed.itemBackpack.itemID) {
				return stack;
			}
		}
		return null;
	}

	private boolean checkIfOnlyContainsBagAndDye(InventoryCrafting inv) {
		int numberNull = 0;
		int numberBags = 0;
		int numberDyes = 0;
		
		for (int i = 0; i < inv.getSizeInventory(); i++) {
			ItemStack slotStack = inv.getStackInSlot(i);
			if (slotStack == null) {
				numberNull++;
			} else if (slotStack.itemID == ProjectRed.itemBackpack.itemID) {
				numberBags++;
			} else if (slotStack.itemID == Item.dyePowder.itemID) {
				numberDyes++;
			}
		}
		
		return (numberNull == inv.getSizeInventory() - 2) && numberBags == 1 && numberDyes == 1;
	}

	@Override
	public int getRecipeSize() {
		return 2;
	}

	@Override
	public ItemStack getRecipeOutput() {
		return new ItemStack(ProjectRed.itemBackpack, 1, Short.MAX_VALUE);
	}

}
