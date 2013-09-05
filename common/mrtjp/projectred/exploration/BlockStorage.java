package mrtjp.projectred.exploration;

import java.util.List;

import static net.minecraftforge.common.ForgeDirection.UP;
import mrtjp.projectred.ProjectRedExploration;

import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.Icon;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.Entity;
import net.minecraft.item.ItemStack;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldProviderEnd;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockStorage extends Block {
	public BlockStorage(int id) {
		super(id, Material.rock);
		setResistance(10.0F);
		setHardness(1.8F);
        setCreativeTab(ProjectRedExploration.tabExploration);
	}

	@Override
	public boolean isFireSource(World world, int x, int y, int z, int metadata, ForgeDirection side) {
		if (metadata == 0 && side == UP) {
			return true;
		}
		return false;
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
		if (metadata <= 2) {
			return iconBuffer[metadata];
		}
		return blockIcon;
	}

	@Override
	public int damageDropped(int metadata) {
		return metadata;
	}

	@SideOnly(Side.CLIENT)
	public void getSubBlocks(int par1, CreativeTabs tab, List subItems) {
		for (int i = 0; i < 2; i++) {
			subItems.add(new ItemStack(this, 1, i));
		}
	}
}
