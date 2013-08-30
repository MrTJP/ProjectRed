package mrtjp.projectred.exploration;

import java.util.List;

import mrtjp.projectred.ProjectRedExploration;

import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;

public class ItemBlockStorage extends ItemBlock {
	private final static String[] subNames = {
		"Ruby Block",
		"Sapphire Block",
		"Peridot Block",
		"", "", "", "", "", "", "", "", "", "", "", "", ""
	};

	public ItemBlockStorage(int id) {
		super(id);
		setHasSubtypes(true);
		// setItemName("storageblock");
	}

	@Override
	public int getMetadata(int i) {
		return i;
	}

	@Override
	// public String getItemNameIS(ItemStack itemstack) {
	// return getItemName() + "." + subNames[itemstack.getItemDamage()];
	// }
	public String getUnlocalizedName(ItemStack itemstack) {
		return subNames[itemstack.getItemDamage()];
		// getItemName() + "." +
	}
}
