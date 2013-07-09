package mrtjp.projectred.blocks;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.crafting.ProjectRedTabs;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.oredict.OreDictionary;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockLamp extends Block {

	public static Icon[] onIcons = new Icon[16];
	public static Icon[] offIcons = new Icon[16];

	public BlockLamp(int id) {
		super(id, new Material(Material.circuits.materialMapColor));
		setBlockBounds(0.0F, 0.0F, 0.0F, 1.0F, 1.0F, 1.0F);
		setHardness(0.5F);
		setCreativeTab(ProjectRedTabs.tabLighting);
	}

	@Override
	public void onNeighborBlockChange(World world, int x, int y, int z, int id) {
		TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLamp.class);
		if (tile != null) {
			tile.onNeighborBlockChange();
		}
	}

	@Override
	public boolean renderAsNormalBlock() {
		return true;
	}

	@Override
	public int getRenderType() {
		return 0;
	}

	@Override
	public boolean isOpaqueCube() {
		return true;
	}

	@Override
	public boolean isBlockNormalCube(World world, int x, int y, int z) {
		return true;
	}

	@Override
	public void getSubBlocks(int id, CreativeTabs tab, List list) {
		for (int i = 0; i < 32; i++) {
			list.add(new ItemStack(id, 1, i));
		}
	}

	@Override
	public int getLightValue(IBlockAccess world, int x, int y, int z) {
		TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLamp.class);
		if (tile != null) {
			return tile.getLightValue();
		}
		return 0;
	}

	@Override
	public boolean canCreatureSpawn(EnumCreatureType type, World world, int x, int y, int z) {
		return false;
	}

	@Override
	public boolean canConnectRedstone(IBlockAccess world, int x, int y, int z, int side) {
		return true;
	}

	@Override
	public boolean canProvidePower() {
		return true;
	}

	@Override
	public void onBlockAdded(World world, int x, int y, int z) {
		TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLamp.class);
		if (tile != null) {
			tile.onBlockAdded();
		}
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void registerIcons(IconRegister reg) {
		for (int i = 0; i < 16; i++) {
			onIcons[i] = reg.registerIcon("projectred:" + "lampon/" + EnumLamp.get(i).unlocalName + "on");
			offIcons[i] = reg.registerIcon("projectred:" + "lampoff/" + EnumLamp.get(i).unlocalName + "off");
		}
	}

	@Override
	public ItemStack getPickBlock(MovingObjectPosition target, World world, int x, int y, int z) {
		TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLamp.class);
		if (tile != null) {
			return tile.getDroppedBlock();
		}
		return null;
	}

	@Override
	public boolean removeBlockByPlayer(World world, EntityPlayer player, int x, int y, int z) {
		TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLamp.class);
		if (tile != null && !player.capabilities.isCreativeMode) {
			BasicUtils.dropItem(world, x, y, z, tile.getDroppedBlock());
		}
		return super.removeBlockByPlayer(world, player, x, y, z);
	}

	public ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int metadata, int fortune) {
		return new ArrayList<ItemStack>(); // Handled on removeBlockByPlayer
	}

	@Override
	public Icon getBlockTexture(IBlockAccess world, int x, int y, int z, int side) {
		TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLamp.class);
		if (tile != null) {
			if (tile.getLightValue() == 15) {
				return onIcons[tile.getColor()];
			} else {
				return offIcons[tile.getColor()];
			}
		}
		return super.getBlockTexture(world, x, y, z, side);
	}

	/**
	 * From the specified side and block metadata retrieves the blocks texture.
	 * Args: side, metadata
	 */
	public Icon getIcon(int side, int meta) {
		if (meta > 15) {
			return onIcons[meta - 16];
		} else {
			return offIcons[meta];
		}
	}

	@Override
	public TileEntity createTileEntity(World world, int meta) {
		return new TileLamp();
	}

	@Override
	public boolean hasTileEntity(int meta) {
		return true;
	}

	@SideOnly(Side.CLIENT)
	@Override
	public int getRenderBlockPass() {
		return 0;
	}

	@Override
	public boolean canRenderInPass(int pass) {
		return pass == 0 || pass == 1;
	}

	public enum EnumLamp {
		WHITE("White Lamp", "lampwhite"),
		ORANGE("Orange Lamp", "lamporange"),
		MAGENTA("Magenta Lamp", "lampmagenta"),
		LIGHTBLUE("Light Blue Lamp", "lamplightblue"),
		YELLOW("Yellow Lamp", "lampyellow"),
		LIME("Lime Lamp", "lamplime"),
		PINK("Pink Lamp", "lamppink"),
		GREY("Grey Lamp", "lampgrey"),
		LIGHTGREY("Light Grey Lamp", "lamplightgrey"),
		CYAN("Cyan Lamp", "lampcyan"),
		PURPLE("Purple Lamp", "lamppurple"),
		BLUE("Blue Lamp", "lampblue"),
		BROWN("Brown Lamp", "lampbrown"),
		GREEN("Green Lamp", "lampgreen"),
		RED("Red Lamp", "lampred"),
		BLACK("Black Lamp", "lampblack"),

		INVALID("ERROR INVALID LAMP", "INVALID");

		public final String fullName;
		public final String unlocalName;
		public static final EnumLamp[] VALID_TYPES = { WHITE, ORANGE, MAGENTA, LIGHTBLUE, YELLOW, LIME, PINK, GREY, LIGHTGREY, CYAN, PURPLE, BLUE, BROWN, GREEN, RED, BLACK };
		public int meta = this.ordinal();
		public static final String oreDictDefinition = "projredLamp";

		private EnumLamp(String name, String unlocal) {
			fullName = name;
			unlocalName = unlocal;
		}

		public static EnumLamp get(int i) {
			if (i > VALID_TYPES.length - 1) {
				return INVALID;
			}
			return VALID_TYPES[i];
		}

		public ItemStack getItemStack() {
			return new ItemStack(ProjectRed.blockLamp.blockID, 1, meta);
		}

		public ItemStack getInvertedItemStack() {
			return new ItemStack(ProjectRed.blockLamp.blockID, 1, meta + 16);
		}

		public static void initOreDictDefinitions() {
			for (EnumLamp l : EnumLamp.VALID_TYPES) {
				OreDictionary.registerOre(oreDictDefinition, l.getItemStack());
			}
		}
	}

}
