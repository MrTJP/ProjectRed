package mrtjp.projectred.crafting.microblocks;

import java.util.HashMap;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.multipart.microblocks.ItemMicroblock;
import mrtjp.projectred.utils.BlockMetaPair;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.world.World;

public class RecipeHorizontalCut implements IRecipe {

	private static HashMap<BlockMetaPair, ItemStack> partIDMap = new HashMap<BlockMetaPair, ItemStack>();

	public static void addMap(BlockMetaPair a, ItemStack b) {
		partIDMap.put(a, b);
	}

	@Override
	public boolean matches(InventoryCrafting inventorycrafting, World world) {
		return getCraftingResult(inventorycrafting) != null;
	}

	@Override
	public ItemStack getCraftingResult(InventoryCrafting inventorycrafting) {
		int n = 0;
		for (int k = 0; k < inventorycrafting.getSizeInventory(); k++) {
			if (inventorycrafting.getStackInSlot(k) != null)
				n++;
		}
		if (n != 2)
			return null;

		ItemStack is;
		for (int i = 0; i <= 1; i++)
			for (int j = 0; j <= 2; j++) {
				is = check(inventorycrafting, i, j, true);
				if (is != null)
					return is;
				is = check(inventorycrafting, i, j, false);
				if (is != null)
					return is;
			}
		return null;
	}

	private ItemStack check(InventoryCrafting i, int xo, int yo, boolean flip) {
		ItemStack saw = i.getStackInRowAndColumn(xo + (flip ? 1 : 0), yo);
		if (saw == null || saw.itemID != ProjectRed.itemSaw.itemID)
			return null;
		ItemStack block = i.getStackInRowAndColumn(xo + (flip ? 0 : 1), yo);
		if (block == null)
			return null;
		int inputMeta = block.itemID == ProjectRed.blockMicrocontainer.blockID ? ItemMicroblock.getPartTypeID(block) : block.getItemDamage();
		return ItemStack.copyItemStack(partIDMap.get(new BlockMetaPair(block.itemID, inputMeta)));
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
