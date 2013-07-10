package mrtjp.projectred.blocks;

import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemBlockLantern extends ItemBlock {

	public ItemBlockLantern(int par1) {
		super(par1);
		setHasSubtypes(true);
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
		String base = Block.blocksList[this.getBlockID()].getUnlocalizedName();
		int color = itemstack.getItemDamage();
		boolean inverted = false;
		if (color > 15) {
			inverted = true;
			color -= 16;
		}
		String invertedDescription = inverted ? "" + "_inverted" : "";
		return base + "_" + EnumLantern.get(color).unlocalName + invertedDescription;
	}

	@Override
	public boolean canPlaceItemBlockOnSide(World world, int x, int y, int z, int side, EntityPlayer par6EntityPlayer, ItemStack par7ItemStack) {
        Block supporter = Block.blocksList[world.getBlockId(x, y, z)];
       if (supporter != null && supporter.isBlockSolidOnSide(world, x, y, z, ForgeDirection.getOrientation(side))) {
    	   return true;
       } else if (supporter.isNormalCube(supporter.blockID) || supporter.blockID == Block.glass.blockID) {
    	   return true;
       }
       return false;
	}

	@Override
	public boolean placeBlockAt(ItemStack stack, EntityPlayer player, World world, int x, int y, int z, int side, float hitX, float hitY, float hitZ, int metadata) {
		int meta = stack.getItemDamage();
		if (meta < 0 || meta >= 32) {
			return false;
		}
		if (!super.placeBlockAt(stack, player, world, x, y, z, side, hitX, hitY, hitZ, metadata)) {
			return false;
		}
		if (world.getBlockId(x, y, z) == itemID) {
			world.setBlockTileEntity(x, y, z, new TileLantern(meta, meta > 15 ? true : false, ForgeDirection.OPPOSITES[side]));
		}
		return true;
	}

	@SideOnly(Side.CLIENT)
	@Override
	/**
	 * Gets an icon index based on an item's damage value
	 */
	public Icon getIconFromDamage(int meta) {
		if (meta < BlockLantern.iconsForBreakEffect.length) {
			return BlockLantern.iconsForBreakEffect[meta];
		}
		return null;
	}

}
