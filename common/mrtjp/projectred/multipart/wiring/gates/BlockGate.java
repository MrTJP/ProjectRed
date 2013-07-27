package mrtjp.projectred.multipart.wiring.gates;

import java.util.List;
import java.util.Random;

import mrtjp.projectred.crafting.ProjectRedTabs;
import mrtjp.projectred.multipart.BlockMultipartBase;
import mrtjp.projectred.network.ClientProxy;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.BasicWireMaterial;
import mrtjp.projectred.utils.Coords;
import mrtjp.projectred.utils.Dir;
import mrtjp.projectred.utils.codechicken.core.vec.Rotation;
import mrtjp.projectred.utils.codechicken.core.vec.Translation;
import mrtjp.projectred.utils.codechicken.core.vec.Vector3;
import net.minecraft.block.Block;
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
		super(id, new BasicWireMaterial());
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

	@Override
	public int getRenderBlockPass() {
		return 1;
	}

	@Override
	public boolean canRenderInPass(int pass) {
		ClientProxy.renderPass = pass;
		return true;
	}

	@SideOnly(Side.CLIENT)
	@Override
	public void randomDisplayTick(World world, int x, int y, int z, Random ran) {
		if (BasicUtils.isServer(world)) {
			return;
		}
		TileGate gate = (TileGate) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileGate.class);
		if (gate != null) {
			GateRenderBridge bridge = gate.getType().getRenderBridge();
			bridge.set(gate.getRenderState());
			for (int i = 0; i < bridge.torchState.length; i++) {
				if (!bridge.torchState[i]) {
					continue;
				}
				float xOffset = ((16f - bridge.torchX[i]) / 16f);
				float yOffset = (bridge.torchY[i] + 6) / 16f;
				float zOffset = ((16f - bridge.torchZ[i]) / 16f);
				Vector3 vec = new Vector3();
				vec.apply(new Translation(xOffset, yOffset, zOffset));
				vec.apply(Rotation.getForSideFacing(gate.getSide(), gate.getFront()));
				vec.apply(new Translation(x, y, z));
				double d0 = (double) ((float) vec.x) + (double) (ran.nextFloat() - .5F) * 0.02D;
				double d1 = (double) ((float) vec.y) + (double) (ran.nextFloat() - .3F) * 0.3D;
				double d2 = (double) ((float) vec.z) + (double) (ran.nextFloat() - .5F) * 0.02D;
				world.spawnParticle("reddust", d0, d1, d2, 0.0D, 0.0D, 0.0D);
			}
			for (int i = 0; i < bridge.pointerX.length; i++) {
				float xOffset = ((16f - bridge.pointerX[i]) / 16f);
				float yOffset = (bridge.pointerY[i] + 6) / 16f;
				float zOffset = ((16f - bridge.pointerZ[i]) / 16f);
				Vector3 vec = new Vector3();
				vec.apply(new Translation(xOffset, yOffset, zOffset));
				vec.apply(Rotation.getForSideFacing(gate.getSide(), gate.getFront()));
				vec.apply(new Translation(x, y, z));
				double d0 = (double) ((float) vec.x) + (double) (ran.nextFloat() - .5F) * 0.02D;
				double d1 = (double) ((float) vec.y) + (double) (ran.nextFloat() - .3F) * 0.3D;
				double d2 = (double) ((float) vec.z) + (double) (ran.nextFloat() - .5F) * 0.02D;
				world.spawnParticle("reddust", d0, d1, d2, 0.0D, 0.0D, 0.0D);
			}
		}
	};

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
		if (BasicUtils.isServer(w)) {
			TileGate tile = (TileGate) BasicUtils.getTileEntity(w, new Coords(x, y, z), TileGate.class);
			if (tile != null) {
				tile.onNeighborChanged();
				if ((id == 0 || Block.blocksList[id].canProvidePower())) {
					tile.updateLogic(false, false);
				}
			}
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
		GateRenderBridge.registerAllIcons(reg);
		blockIcon = reg.registerIcon("projectred:gates/base");
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
		TileGate te = (TileGate) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileGate.class);
		if (te != null) {
			EnumGate type = te.getType();
			if (type != null) {
				GateRenderBridge render = type.getRenderBridge();
				int on = 0;
				if (render != null) {
					on += render.torchState.length;
					on += render.pointerX.length;
				}
				return on > 0 ? on + 4 : 0;
			}
		}
		return 0;
	}

}
