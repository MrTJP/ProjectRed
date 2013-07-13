package mrtjp.projectred.multipart.wiring.gates;

import java.util.List;
import java.util.Random;

import mrtjp.projectred.crafting.ProjectRedTabs;
import mrtjp.projectred.multipart.BlockMultipartBase;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.material.MaterialLogic;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockGate extends BlockMultipartBase {

	public static final float THICKNESS = 1 / 8.0f;

	public BlockGate(int id) {
		super(id, new MaterialLogic(Material.circuits.materialMapColor));
		setCreativeTab(ProjectRedTabs.tabWires);
		setUnlocalizedName("projred.gate");
	}

	// BEGIN RENDERING
	static Icon textureOverride = null;
	static int colourOverride = -1;
	static int renderSide;

	@Override
	@SideOnly(Side.CLIENT)
	public Icon getIcon(int side, int meta) {
		if (textureOverride != null)
			return textureOverride;
		else
			return super.getIcon(side, meta);
	}

	@Override
	public int getRenderColor(int par1) {
		if (colourOverride < 0)
			return super.getRenderColor(par1);
		else
			return colourOverride;
	}

	@Override
	public boolean shouldSideBeRendered(IBlockAccess par1iBlockAccess, int par2, int par3, int par4, int par5) {
		if (renderSide < 0)
			return par5 != -(renderSide + 1);
		else
			return par5 == renderSide;
	}

	@Override
	public int colorMultiplier(IBlockAccess par1iBlockAccess, int par2, int par3, int par4) {
		if (colourOverride >= 0)
			return colourOverride;
		else
			return super.colorMultiplier(par1iBlockAccess, par2, par3, par4);
	}

	static int renderTypeOverride = -1;

	@Override
	public int wrappedGetRenderType() {
		return renderTypeOverride >= 0 ? renderTypeOverride : RenderIDs.renderIdGate;
	}

	// END RENDERING

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	@SideOnly(Side.CLIENT)
	public void getSubBlocks(int id, CreativeTabs tab, List list) {
		for (EnumGate type : EnumGate.VALUES) {
			list.add(new ItemStack(this, 1, type.ordinal()));
		}
	}

	@Override
	public TileEntity createNewTileEntity(World world) {
		return new TileGate();
	}

	@Override
	public boolean canProvidePower() {
		return true;
	}

	@Override
	public boolean canConnectRedstone(IBlockAccess world, int x, int y, int z, int side) {
		return true;
	}

	@Override
	public void onNeighborBlockChange(World w, int x, int y, int z, int id) {
		super.onNeighborBlockChange(w, x, y, z, id);

		if ((id == 0 || Block.blocksList[id].canProvidePower()) && !w.isRemote) {
			((TileGate) w.getBlockTileEntity(x, y, z)).updateLogic(false, false);
		}
	}

	@Override
	public int isProvidingStrongPower(IBlockAccess par1iBlockAccess, int par2, int par3, int par4, int par5) {
		return ((TileGate) par1iBlockAccess.getBlockTileEntity(par2, par3, par4)).getVanillaOutputStrength(par5 ^ 1);
	}

	@Override
	public int isProvidingWeakPower(IBlockAccess par1iBlockAccess, int par2, int par3, int par4, int par5) {
		return ((TileGate) par1iBlockAccess.getBlockTileEntity(par2, par3, par4)).getVanillaOutputStrength(par5 ^ 1);
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void registerIcons(IconRegister reg) {
		for (EnumGate type : EnumGate.VALUES) {
			type.getRendering().loadTextures(reg);
		}
		blockIcon = reg.registerIcon("projectred:base");
	}

	private int getSide(IBlockAccess w, int x, int y, int z) {
		TileEntity te = w.getBlockTileEntity(x, y, z);
		if (!(te instanceof TileGate)) {
			return Dir.NY;
		}
		return ((TileGate) te).getSide();
	}

	@Override
	public void setBlockBoundsBasedOnState(IBlockAccess w, int x, int y, int z) {
		switch (getSide(w, x, y, z)) {
		case Dir.NX:
			setBlockBounds(0, 0, 0, BlockGate.THICKNESS, 1, 1);
			break;
		case Dir.NY:
			setBlockBounds(0, 0, 0, 1, BlockGate.THICKNESS, 1);
			break;
		case Dir.NZ:
			setBlockBounds(0, 0, 0, 1, 1, BlockGate.THICKNESS);
			break;
		case Dir.PX:
			setBlockBounds(1 - BlockGate.THICKNESS, 0, 0, 1, 1, 1);
			break;
		case Dir.PY:
			setBlockBounds(0, 1 - BlockGate.THICKNESS, 0, 1, 1, 1);
			break;
		case Dir.PZ:
			setBlockBounds(0, 0, 1 - BlockGate.THICKNESS, 1, 1, 1);
			break;
		}
	}

	@Override
	public AxisAlignedBB getCollisionBoundingBoxFromPool(World w, int x, int y, int z) {
		switch (getSide(w, x, y, z)) {
		case Dir.NX:
			return AxisAlignedBB.getAABBPool().getAABB(0, 0, 0, BlockGate.THICKNESS, 1, 1).offset(x, y, z);
		case Dir.NY:
			return AxisAlignedBB.getAABBPool().getAABB(0, 0, 0, 1, BlockGate.THICKNESS, 1).offset(x, y, z);
		case Dir.NZ:
			return AxisAlignedBB.getAABBPool().getAABB(0, 0, 0, 1, 1, BlockGate.THICKNESS).offset(x, y, z);
		case Dir.PX:
			return AxisAlignedBB.getAABBPool().getAABB(1 - BlockGate.THICKNESS, 0, 0, 1, 1, 1).offset(x, y, z);
		case Dir.PY:
			return AxisAlignedBB.getAABBPool().getAABB(0, 1 - BlockGate.THICKNESS, 0, 1, 1, 1).offset(x, y, z);
		case Dir.PZ:
			return AxisAlignedBB.getAABBPool().getAABB(0, 0, 1 - BlockGate.THICKNESS, 1, 1, 1).offset(x, y, z);
		}
		return null;
	}

	@Override
	public void updateTick(World w, int x, int y, int z, Random par5Random) {
		((TileGate) w.getBlockTileEntity(x, y, z)).scheduledTick();
	}

	@Override
	public boolean onBlockActivated(World par1World, int par2, int par3, int par4, EntityPlayer par5EntityPlayer, int par6, float par7, float par8, float par9) {
		return ((TileGate) par1World.getBlockTileEntity(par2, par3, par4)).onBlockActivated(par5EntityPlayer);
	}

	@Override
	public int getLightValue(IBlockAccess world, int x, int y, int z) {
		return 7;
	}

}
