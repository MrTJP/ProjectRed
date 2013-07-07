package mrtjp.projectred.blocks;

import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.crafting.ProjectRedTabs;
import mrtjp.projectred.tiles.TileAlloySmelter;
import mrtjp.projectred.tiles.TileMachineBase;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.block.Block;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockMachines extends BlockContainer {

	public Icon[][] machineIcons = new Icon[EnumMachine.VALID_MACHINES.length][6];

	public Icon[][] specialIcons = new Icon[EnumMachine.VALID_MACHINES.length][6];

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
	public void onBlockPlacedBy(World world, int x, int y, int z, EntityLiving entity, ItemStack itemstack) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {
			tile.onBlockPlaced(entity, itemstack);
		}
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
			for (int i = 0; i < 6; i++) {
				machineIcons[m.meta][i] = reg.registerIcon("projectred:machines/" + m.iconPath[i]);
				specialIcons[m.meta][i] = reg.registerIcon("projectred:machines/" + m.specialIconPath[i]);
			}
		}
	}

	@SideOnly(Side.CLIENT)
	@Override
	public Icon getBlockTexture(IBlockAccess access, int x, int y, int z, int side) {
		TileMachineBase tile = (TileMachineBase) BasicUtils.getTileEntity(access, new Coords(x, y, z), TileMachineBase.class);
		if (tile != null) {

			return getRotatedTexture(side, access.getBlockMetadata(x, y, z), tile.rotation, tile);
		}
		return null;
	}

	@SideOnly(Side.CLIENT)
	@Override
	public Icon getIcon(int side, int meta) {
		return getRotatedTexture(side, meta, 2, null);
	}

	/**
	 * Side is ForgeDirection ordinal. In inventory, you want to render the
	 * south side of the block as front. The tile will be asked to apply special
	 * texture, if it returns false, then default will go. The tile param CAN be
	 * null;
	 * 
	 * @param meta
	 * @param side
	 * @param rotation
	 * @param tile
	 * @return
	 */
	public Icon getRotatedTexture(int side, int meta, int rotation, TileMachineBase tile) {
		if (meta > EnumMachine.VALID_MACHINES.length - 1 || side > 6 || rotation > 6) {
			return null;
		}
		boolean useSpecial = false;
		if (tile != null) {
			useSpecial = tile.shouldUseSpecialTextureForSide(side);
		}
		if (side == 0) {
			return useSpecial ? specialIcons[meta][0] : machineIcons[meta][0]; // down
																				// is
																				// rotation
																				// independent
		}
		if (side == 1) {
			return useSpecial ? specialIcons[meta][1] : machineIcons[meta][1]; // up
																				// is
																				// rotation
																				// independent
		}
		// We will first see what side minecraft is asking for, then
		// we will provide the right texture based on the orientation
		// of the block.
		switch (side) {
		case 2: // icons when we want north side
			switch (rotation) {
			case 2: // north side when rotated north
				return useSpecial ? specialIcons[meta][2] : machineIcons[meta][2];
			case 3: // south side when rotated north
				return useSpecial ? specialIcons[meta][3] : machineIcons[meta][3];
			case 4: // west side when rotated north
				return useSpecial ? specialIcons[meta][4] : machineIcons[meta][4];
			case 5: // east side when rotated north
				return useSpecial ? specialIcons[meta][5] : machineIcons[meta][5];
			}
		case 3: // icons when we want south side
			switch (rotation) {
			case 2: // north side when rotated south
				return useSpecial ? specialIcons[meta][3] : machineIcons[meta][3];
			case 3: // south side when rotated south
				return useSpecial ? specialIcons[meta][2] : machineIcons[meta][2];
			case 4: // west side when rotated south
				return useSpecial ? specialIcons[meta][5] : machineIcons[meta][5];
			case 5: // east side when rotated south
				return useSpecial ? specialIcons[meta][4] : machineIcons[meta][4];
			}
		case 4: // icons when we want west side
			switch (rotation) {
			case 2: // north side when rotated west
				return useSpecial ? specialIcons[meta][5] : machineIcons[meta][5];
			case 3: // south side when rotated west
				return useSpecial ? specialIcons[meta][4] : machineIcons[meta][4];
			case 4: // west side when rotated west
				return useSpecial ? specialIcons[meta][2] : machineIcons[meta][2];
			case 5: // east side when rotated west
				return useSpecial ? specialIcons[meta][3] : machineIcons[meta][3];
			}
		case 5: // icons when we want east side
			switch (rotation) {
			case 2: // north side when rotated east
				return useSpecial ? specialIcons[meta][4] : machineIcons[meta][4];
			case 3: // south side when rotated east
				return useSpecial ? specialIcons[meta][5] : machineIcons[meta][5];
			case 4: // west side when rotated east
				return useSpecial ? specialIcons[meta][3] : machineIcons[meta][3];
			case 5: // east side when rotated east
				return useSpecial ? specialIcons[meta][2] : machineIcons[meta][2];
			}
		}
		return null;
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
		ALLOYSMELTER("Alloy Smelter", "machinealloy", TileAlloySmelter.class, "smeltertop", "smeltertop", "smelterside", "smelterfront", "smelterside", "smelterside", "smeltertop", "smeltertop", "smelterside", "smelterfronton", "smelterside", "smelterside"),
		BARREL("Barrel", "machinebarrel", null, "", "", "", "", "", "", "", "", "", "", "", ""),

		INVALIDMACHINE("INVALID", "ERROR!!!", null, "", "", "", "", "", "", "", "", "", "", "", "");
		;

		public static final EnumMachine[] VALID_MACHINES = { ALLOYSMELTER };

		public String fullname;
		public String unlocalname;
		public Class<? extends TileMachineBase> clazz;
		public int meta = this.ordinal();

		public String[] iconPath = new String[6];
		public String[] specialIconPath = new String[6];

		private EnumMachine(String name, String unlocal, Class<? extends TileMachineBase> tile, String... sides) {
			fullname = name;
			unlocalname = unlocal;
			clazz = tile;

			// Icon paths are the name of the icon only. The folder is always
			// ...textures/blocks/machines/
			iconPath[0] = sides[0];
			iconPath[1] = sides[1];
			iconPath[2] = sides[2];
			iconPath[3] = sides[3]; // This is the front texture
			iconPath[4] = sides[4];
			iconPath[5] = sides[5];

			specialIconPath[0] = sides[6];
			specialIconPath[1] = sides[7];
			specialIconPath[2] = sides[8];
			specialIconPath[3] = sides[9]; // This is the front texture
			specialIconPath[4] = sides[10];
			specialIconPath[5] = sides[11];
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
	}

}
