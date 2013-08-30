package mrtjp.projectred.exploration;

import java.util.List;

import static net.minecraftforge.common.ForgeDirection.UP;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.ProjectRedTabs;

import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.Icon;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.Entity;
import net.minecraft.item.ItemStack;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldProviderEnd;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockStorage extends Block {
	public BlockStorage(int id) {
		super(id, Material.rock);
		setResistance(10.0F);
        setCreativeTab(ProjectRedTabs.tabExploration);
	}

	@Override
	public boolean isFireSource(World world, int x, int y, int z, int metadata,
			ForgeDirection side) {
		if (metadata == 0 && side == UP) {
			return true;
		}
		return false;
	}

	@Override
	public float getBlockHardness(World par1World, int par2, int par3, int par4) {
		// return !isActive(par1World.getBlockMetadata(par2, par3, par4)) ? 0 :
		// 3;
		int metadata = par1World.getBlockMetadata(par2, par3, par4);
		if (metadata == 0)
			return 1.8f;
		if (metadata == 1)
			return 1.8f;
		if (metadata == 2)
			return .8f;
		if (metadata == 3)
			return .8f;
		if (metadata == 4)
			return .8f;
		if (metadata == 5)
			return .8f;
		if (metadata == 6)
			return .8f;
		if (metadata == 7)
			return .8f;
		if (metadata == 8)
			return .8f;

		return 2f;
	}

	@Override
	public void onEntityCollidedWithBlock(World world, int x, int y, int z,
			Entity entity) {
		int meta = world.getBlockMetadata(x, y, z);
		if (meta == 2) {
			if (entity.motionY < 0)
				entity.motionY *= -1.2F;
			entity.fallDistance = 0;
		}
	}

	@Override
	public AxisAlignedBB getCollisionBoundingBoxFromPool(World world, int x,
			int y, int z) {
		int meta = world.getBlockMetadata(x, y, z);
		if (meta == 2)
			return AxisAlignedBB.getBoundingBox(x, y, z, (double) x + 1.0D,
					(double) y + 0.625D, (double) z + 1.0D);
		return super.getCollisionBoundingBoxFromPool(world, x, y, z);
	}

	private Icon[] iconBuffer;

	@Override
	public void registerIcons(IconRegister par1IconRegister) {
		iconBuffer = new Icon[9];

		iconBuffer[0] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[1] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[2] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[3] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[4] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[5] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[6] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[7] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
		iconBuffer[8] = par1IconRegister.registerIcon("projectred:ore/brickmarble");
	}

	@Override
	// public Icon getBlockTextureFromSideAndMetadata (int side, int metadata) {
	public Icon getIcon(int side, int metadata) {

		if (metadata == 0) {
			return iconBuffer[0];
		}
		if (metadata == 1) {
			return iconBuffer[1];
		}
		if (metadata == 2) {
			return iconBuffer[2];
		}
		if (metadata == 3) {
			return iconBuffer[3];
		}
		if (metadata == 4) {
			return iconBuffer[4];
		}
		if (metadata == 5) {
			switch (side) {
			case 0:
				return iconBuffer[7];
			case 1:
				return iconBuffer[7];
			default:
				return iconBuffer[8];
			}
		}
		if (metadata == 6) {
			return iconBuffer[5];
		}
		if (metadata == 7) {
			return iconBuffer[6];
		}
		return blockIcon;
	}

	@Override
	public int damageDropped(int metadata) {
		return metadata;
	}

	@SideOnly(Side.CLIENT)
	public void getSubBlocks(int par1, CreativeTabs tab, List subItems) {
		for (int ix = 0; ix < 8; ix++) {
			subItems.add(new ItemStack(this, 1, ix));
		}
	}
}
