package mrtjp.projectred.blocks;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.crafting.ProjectRedTabs;
import mrtjp.projectred.tiles.TileAlloySmelter;
import mrtjp.projectred.tiles.TileMachineBase;
import mrtjp.projectred.tiles.TileTurbineRotary;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockMachines extends BlockContainer {

	public BlockMachines(int id) {
		super(id, new Material(Material.iron.materialMapColor));
		setCreativeTab(ProjectRedTabs.tabMachines);
		setHardness(0.75f);
	}

	@Override
	public TileEntity createNewTileEntity(World world) {
		return null;
	}

	@Override
	public TileEntity createTileEntity(World world, int metadata) {
		try {
			return EnumMachine.get(metadata).clazz.newInstance();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public boolean hasTileEntity(int meta) {
		return true;
	}

	@Override
	public void breakBlock(World world, int x, int y, int z, int par5, int par6) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {
			tile.onBlockBreak();
		}
		super.breakBlock(world, x, y, z, par5, par6);
		world.removeBlockTileEntity(x, y, z);
	}

	@Override
	public void onBlockClicked(World world, int x, int y, int z, EntityPlayer player) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {
			tile.onBlockClicked(player);
		}
	}

	@Override
	public boolean onBlockActivated(World world, int x, int y, int z, EntityPlayer player, int par6, float par7, float par8, float par9) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {
			return tile.onBlockActivated(player);
		}
		return false;
	}

	@Override
	public void onBlockPlacedBy(World world, int x, int y, int z, EntityLivingBase entity, ItemStack itemstack) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {
			tile.onBlockPlacedBy(entity, itemstack);
		}
	}
	
	@Override
	public final ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int metadata, int fortune) {
		return new ArrayList<ItemStack>();
	}

	@Override
	public void getSubBlocks(int id, CreativeTabs tab, List list) {
		for (EnumMachine machine : EnumMachine.VALID_MACHINES) {
			list.add(new ItemStack(id, 1, machine.meta));
		}
	}

	@Override
	public void registerIcons(IconRegister reg) {
		for (EnumMachine m : EnumMachine.VALID_MACHINES) {
			m.icons = new Icon[m.iconPath.length];
			for (int i = 0; i < m.iconPath.length; i++) {
				m.icons[i] = reg.registerIcon("projectred:machines/" + m.iconPath[i]);
			}
		}
	}

	@SideOnly(Side.CLIENT)
	@Override
	public Icon getBlockTexture(IBlockAccess access, int x, int y, int z, int side) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(access, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {
			return tile.getType().icons[tile.getIconForSide(side)];
		}
		return null;
	}

	@SideOnly(Side.CLIENT)
	@Override
	public Icon getIcon(int side, int meta) {
		return EnumMachine.get(meta).icons[EnumMachine.get(meta).textProvider.getIconIndex(side)];
	}


	@Override
	public int getLightValue(IBlockAccess world, int x, int y, int z) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {
			return tile.getLightLevel();
		}
		return 0;
	}

	public enum EnumMachine {
		ALLOYSMELTER("Alloy Smelter", "machinealloy", TileAlloySmelter.class, new IIconIndexer() {
			@Override
			public int getIconIndex(int side) {
				return side == 0 || side == 1 ? 0 : side == 3 ? 2 : 1;
			}
		}, "smeltertop", "smelterside", "smelterfront", "smelterfronton"),
		
		TURBINEROTARY("Wind Turbine Rotary", "machinewindgenerator", TileTurbineRotary.class, new IIconIndexer() {
			@Override
			public int getIconIndex(int side) {
				return side == 0 ? 2 : side == 1 ? 0 : 1;
			}
		},"generatortop", "generatorside", "generatorbottom", "generatorsideon"),
		
		BARREL("Barrel", "machinebarrel", null, null),
		INVALIDMACHINE("INVALID", "ERROR!!!", null, null),
		;

		public static final EnumMachine[] VALID_MACHINES = { ALLOYSMELTER, TURBINEROTARY };

		public String fullname;
		public String unlocalname;
		public Class<? extends TileMachineBase> clazz;
		public int meta = this.ordinal();

		public String[] iconPath = new String[6];
		public Icon[] icons;
		
		public IIconIndexer textProvider;

		private EnumMachine(String name, String unlocal, Class<? extends TileMachineBase> tile, IIconIndexer p, String... sides) {
			fullname = name;
			unlocalname = unlocal;
			clazz = tile;
			iconPath = sides;
			textProvider = p;
		}

		public static EnumMachine get(int ordinal) {
			if (ordinal > VALID_MACHINES.length - 1) {
				return INVALIDMACHINE;
			}
			return VALID_MACHINES[ordinal];
		}
		public ItemStack getItemStack() {
			return getItemStack(1);
		}
		public ItemStack getItemStack(int i) {
			return new ItemStack(ProjectRed.blockMachines, i, meta);
		}
		public interface IIconIndexer {
			public int getIconIndex(int side);
		}
	}
	

}
