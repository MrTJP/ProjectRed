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
	public boolean isFireSource(World world, int x, int y, int z, int metadata, ForgeDirection side) {
		if (metadata == 0 && side == UP) {
			return true;
		}
		return false;
	}

	@Override
	public float getBlockHardness(World par1World, int par2, int par3, int par4) {
		int metadata = par1World.getBlockMetadata(par2, par3, par4);
		if (metadata == 0){
			return 1.8f;
		}
		if (metadata == 1){
			return 1.8f;
		}
		if (metadata == 2){
			return .8f;
		}

		return 2f;
	}

	private Icon[] iconBuffer;

	@Override
	public void registerIcons(IconRegister reg) {
		iconBuffer = new Icon[3];

		iconBuffer[0] = reg.registerIcon("projectred:ore/storageruby");
		iconBuffer[1] = reg.registerIcon("projectred:ore/storagesapphire");
		iconBuffer[2] = reg.registerIcon("projectred:ore/storageperidot");
	}

	@Override
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
		return blockIcon;
	}

	@Override
	public int damageDropped(int metadata) {
		return metadata;
	}

	@SideOnly(Side.CLIENT)
	public void getSubBlocks(int par1, CreativeTabs tab, List subItems) {
		for (int ix = 0; ix < 2; ix++) {
			subItems.add(new ItemStack(this, 1, ix));
		}
	}
}
