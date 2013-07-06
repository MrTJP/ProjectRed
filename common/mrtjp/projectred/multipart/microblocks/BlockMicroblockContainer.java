package mrtjp.projectred.multipart.microblocks;

import mrtjp.projectred.multipart.BlockMultipartBase;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockMicroblockContainer extends BlockMultipartBase {
	public BlockMicroblockContainer(int id, Material mat) {
		super(id, mat);
		setUnlocalizedName("projred.microblock");
	}

	@Override
	public TileEntity createNewTileEntity(World world) {
		return new TileMicroblockContainer();
	}
	
	@Override
	@SideOnly(Side.CLIENT)
	public void registerIcons(IconRegister par1IconRegister) {
	}
	
	@Override
	public boolean canPlaceBlockAt(World par1World, int par2, int par3, int par4)
    {
        int l = par1World.getBlockId(par2, par3, par4);
        Block block = Block.blocksList[l];
        return block == null || block == this || block.isBlockReplaceable(par1World, par2, par3, par4);
    }
}
