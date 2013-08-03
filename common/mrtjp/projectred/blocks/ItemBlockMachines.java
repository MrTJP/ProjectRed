package mrtjp.projectred.blocks;

import mrtjp.projectred.blocks.BlockMachines.EnumMachine;
import mrtjp.projectred.core.ProjectRedTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;

public class ItemBlockMachines extends ItemBlock {

	public ItemBlockMachines(int par1) {
		super(par1);
		setHasSubtypes(true);
		setCreativeTab(ProjectRedTabs.tabExpansion);
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
		return EnumMachine.get(itemstack.getItemDamage()).unlocalname;
	}

	@Override
	public boolean placeBlockAt(ItemStack stack, EntityPlayer player, World world, int x, int y, int z, int side, float hitX, float hitY, float hitZ, int metadata) {
		return super.placeBlockAt(stack, player, world, x, y, z, side, hitX, hitY, hitZ, metadata);
		// let the actual block do this, we will worry about this once
		// we have more than 16 machines in EnumMachine.

		/*
		int meta = stack.getItemDamage();
		if (meta < 0 || meta > EnumMachine.VALID_MACHINES.length - 1) {
			return false;
		}
		if (!super.placeBlockAt(stack, player, world, x, y, z, side, hitX, hitY, hitZ, metadata)) {
			return false;
		}
		if (world.getBlockId(x, y, z) == itemID) {
			try {
				// world.setBlockTileEntity(x, y, z,
				// EnumMachine.get(meta).clazz.newInstance());
			} catch (Exception e) {
				e.printStackTrace();
			}
			return true;
		}
		return false;
		*/
	}

}
