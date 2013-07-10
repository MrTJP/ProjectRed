package mrtjp.projectred.blocks;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.crafting.ProjectRedTabs;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.utils.BasicRenderUtils;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
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

public class BlockLantern extends Block {

	public static Icon[] onIcons = new Icon[16];
	public static Icon[] offIcons = new Icon[16];

	public BlockLantern(int id) {
		super(id, new Material(Material.circuits.materialMapColor));
		setBlockBounds(0.0F, 0.0F, 0.0F, 1.0F, 1.0F, 1.0F);
		setHardness(0.5F);
		setCreativeTab(ProjectRedTabs.tabLighting);
	}
	
	@Override
	public void onNeighborBlockChange(World world, int x, int y, int z, int id) {
		TileLantern tile = (TileLantern) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLantern.class);
		if (tile != null) {
			tile.onNeighborBlockChange();
		}
	}
	@Override
	public boolean renderAsNormalBlock() {
		return false;
	}

	@Override
	public int getRenderType() {
		return -1;
	}

	@Override
	public boolean isOpaqueCube() {
		return false;
	}

	@Override
	public void getSubBlocks(int id, CreativeTabs tab, List list) {
		for (int i = 0; i < 32; i++) {
			list.add(new ItemStack(id, 1, i));
		}
	}

	@Override
	public int getLightValue(IBlockAccess world, int x, int y, int z) {
		TileLantern tile = (TileLantern) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLantern.class);
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
		TileLantern tile = (TileLantern) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLantern.class);
		if (tile != null) {
			tile.onBlockAdded();
		}
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void registerIcons(IconRegister reg) {
		for (int i = 0; i < 16; i++) {
			onIcons[i] = reg.registerIcon("projectred:" + "lanterton/" + EnumLantern.get(i).unlocalName + "on");
			offIcons[i] = reg.registerIcon("projectred:" + "lanternoff/" + EnumLantern.get(i).unlocalName + "off");

		}
	}

	@Override
	public ItemStack getPickBlock(MovingObjectPosition target, World world, int x, int y, int z) {
		TileLantern tile = (TileLantern) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLantern.class);
		if (tile != null) {
			return tile.getDroppedBlock();
		}
		return null;
	}

	@Override
	public boolean removeBlockByPlayer(World world, EntityPlayer player, int x, int y, int z) {
		TileLantern tile = (TileLantern) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLantern.class);
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
		TileLantern tile = (TileLantern) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileLantern.class);
		if (tile != null) {
			int tilemeta = tile.lanternmeta;
			if (tilemeta > 15) {
				tilemeta -= 16;
			}
			if (tile.getLightValue() == 15) {
				return onIcons[tilemeta];
			} else {
				return offIcons[tilemeta];
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
		return new TileLantern();
	}

	@Override
	public boolean hasTileEntity(int meta) {
		return true;
	}

	@SideOnly(Side.CLIENT)
	@Override
	public int getRenderBlockPass() {
		return 1;
	}

	@Override
	public boolean canRenderInPass(int pass) {
		BasicRenderUtils.currentRenderPass = pass;
		return true;
	}

	public enum EnumLantern {
		WHITE("White Lantern", "lanternwhite"),
		ORANGE("Orange Lantern", "lanternorange"),
		MAGENTA("Magenta Lantern", "lanternmagenta"),
		LIGHTBLUE("Light Blue Lantern", "lanternlightblue"),
		YELLOW("Yellow Lantern", "lanternyellow"),
		LIME("Lime Lantern", "lanternlime"),
		PINK("Pink Lantern", "lanternpink"),
		GREY("Grey Lantern", "lanterngrey"),
		LIGHTGREY("Light Grey Lantern", "lanternlightgrey"),
		CYAN("Cyan Lantern", "lanterncyan"),
		PURPLE("Purple Lantern", "lanternpurple"),
		BLUE("Blue Lantern", "lanternblue"),
		BROWN("Brown Lantern", "lanternbrown"),
		GREEN("Green Lantern", "lanterngreen"),
		RED("Red Lantern", "lanternred"),
		BLACK("Black Lantern", "lanternblack"),

		INVALID("ERROR INVALID LANTERN", "INVALID");

		public final String fullName;
		public final String unlocalName;
		public static final EnumLantern[] VALID_TYPES = { WHITE, ORANGE, MAGENTA, LIGHTBLUE, YELLOW, LIME, PINK, GREY, LIGHTGREY, CYAN, PURPLE, BLUE, BROWN, GREEN, RED, BLACK };
		public int meta = this.ordinal();
		public static final String oreDictDefinition = "projredLantern";

		private EnumLantern(String name, String unlocal) {
			fullName = name;
			unlocalName = unlocal;
		}

		public static EnumLantern get(int i) {
			if (i > VALID_TYPES.length - 1) {
				return INVALID;
			}
			return VALID_TYPES[i];
		}

		public ItemStack getItemStack() {
			return new ItemStack(ProjectRed.blockLantern.blockID, 1, meta);
		}

		public ItemStack getInvertedItemStack() {
			return new ItemStack(ProjectRed.blockLantern.blockID, 1, meta + 16);
		}

		public static void initOreDictDefinitions() {
			for (EnumLantern l : EnumLantern.VALID_TYPES) {
				OreDictionary.registerOre(oreDictDefinition, l.getItemStack());
				OreDictionary.registerOre(oreDictDefinition, l.getInvertedItemStack());
			}
		}
	}

}
