package mrtjp.projectred.crafting.microblocks;

import java.util.HashMap;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.multipart.microblocks.ItemBlockMicroblock;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.world.World;

public class RecipeCombineTwo implements IRecipe {

	private static HashMap<Integer, Integer> partIDMap = new HashMap<Integer, Integer>();

	public static void addMap(int from, int to) {
		partIDMap.put(from, to);
	}

	@Override
	public boolean matches(InventoryCrafting var1, World var2) {
		return getCraftingResult(var1) != null;
	}

	@Override
	public ItemStack getCraftingResult(InventoryCrafting var1) {
		ItemStack first = null, second = null;
		for (int k = 0; k < var1.getSizeInventory(); k++) {
			ItemStack is = var1.getStackInSlot(k);
			if (is != null) {
				if (is.itemID != ProjectRed.blockMicrocontainer.blockID)
					return null;

				if (first == null) {
					first = is;
				}
				else if (second == null) {
					second = is;
				}
				else
					return null;
			}
		}

		if (second == null) {
			return null;
		}

		if (ItemBlockMicroblock.getPartTypeID(first) != ItemBlockMicroblock.getPartTypeID(second)) {
			return null;
		}

		Integer newPartID = partIDMap.get(ItemBlockMicroblock.getPartTypeID(first));

		if (newPartID == null) {
			return null;
		}

		return ItemBlockMicroblock.getStackWithPartID(newPartID);

	}

	@Override
	public int getRecipeSize() {
		return 2;
	}

	@Override
	public ItemStack getRecipeOutput() {
		return new ItemStack(ProjectRed.blockMicrocontainer, 1, 0);
	}

}
