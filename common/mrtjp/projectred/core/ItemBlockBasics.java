package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.ProjectRedExpansion;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;

public class ItemBlockBasics extends ItemBlock {

	public ItemBlockBasics(int par1) {
		super(par1);
		setHasSubtypes(true);
		setCreativeTab(ProjectRedCore.tabCore);
	}

	/**
	 * Returns the metadata of the block which this Item (ItemBlock) can place
	 */
	public int getMetadata(int md) {
		return md;
	}

	/**
	 * Returns the unlocalized name of this item. This version accepts an
	 * ItemStack so different stacks can have different names based on their
	 * damage or NBT.
	 */
	@Override
	public String getUnlocalizedName(ItemStack itemstack) {
		return EnumBasics.get(itemstack.getItemDamage()).unlocalname;
	}

}
