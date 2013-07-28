package mrtjp.projectred.multipart.microblocks;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.crafting.microblocks.RecipeCombineSeveral;
import mrtjp.projectred.crafting.microblocks.RecipeCombineTwo;
import mrtjp.projectred.crafting.microblocks.RecipeHollowCover;
import mrtjp.projectred.crafting.microblocks.RecipeHorizontalCut;
import mrtjp.projectred.crafting.microblocks.RecipeUnHollowCover;
import mrtjp.projectred.crafting.microblocks.RecipeVerticalCut;
import mrtjp.projectred.utils.BlockMetaPair;
import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.StringTranslate;
import net.minecraft.world.World;

import com.google.common.base.Strings;
import com.google.common.collect.Sets;

import cpw.mods.fml.common.registry.LanguageRegistry;

public class MicroblockLibrary implements IMicroblockLibrary {

	// Map of all microblock parts.
	public final static HashMap<Integer, PartType<?>> parts = new HashMap<Integer, PartType<?>>();

	public static MicroblockLibrary instance;

	// List of all part ids, used in for loop to add to NEI mostly.
	public static ArrayList<Integer> neiPartIDs = new ArrayList<Integer>();

	public static int neiMaxDamage = 0;

	public void initializeParts() {
		registerManualParts(1, Block.stone);
		registerManualParts(2, Block.grass);
		registerManualParts(3, Block.dirt);
		registerManualParts(4, Block.cobblestone);
		registerManualParts(5, Block.planks, 0);
		registerManualParts(6, Block.planks, 1);
		registerManualParts(7, Block.planks, 2);
		registerManualParts(8, Block.planks, 3);
		registerManualParts(9, Block.bedrock);
		registerManualParts(10, Block.sand);
		registerManualParts(11, Block.gravel);
		registerManualParts(12, Block.oreGold);
		registerManualParts(13, Block.oreIron);
		registerManualParts(14, Block.oreCoal);
		registerManualParts(15, Block.wood, 0);
		registerManualParts(16, Block.wood, 1);
		registerManualParts(17, Block.wood, 2);
		registerManualParts(18, Block.wood, 3);
		// registerManualParts(19, Block.leaves, 0);
		// registerManualParts(20, Block.leaves, 1);
		// registerManualParts(21, Block.leaves, 2);
		// registerManualParts(22, Block.leaves, 3);
		registerManualParts(23, Block.sponge);
		registerManualParts(24, Block.glass);
		registerManualParts(25, Block.oreLapis);
		registerManualParts(26, Block.blockLapis);
		registerManualParts(27, Block.dispenser);
		registerManualParts(28, Block.sandStone);
		registerManualParts(29, Block.music);
		registerManualParts(30, Block.pistonStickyBase);
		registerManualParts(31, Block.pistonBase);
		registerManualParts(32, Block.cloth, 0);
		registerManualParts(33, Block.cloth, 1);
		registerManualParts(34, Block.cloth, 2);
		registerManualParts(35, Block.cloth, 3);
		registerManualParts(36, Block.cloth, 4);
		registerManualParts(37, Block.cloth, 5);
		registerManualParts(38, Block.cloth, 6);
		registerManualParts(39, Block.cloth, 7);
		registerManualParts(40, Block.cloth, 8);
		registerManualParts(41, Block.cloth, 9);
		registerManualParts(42, Block.cloth, 10);
		registerManualParts(43, Block.cloth, 11);
		registerManualParts(44, Block.cloth, 12);
		registerManualParts(45, Block.cloth, 13);
		registerManualParts(46, Block.cloth, 14);
		registerManualParts(47, Block.cloth, 15);
		registerManualParts(48, Block.blockGold);
		registerManualParts(49, Block.blockIron);
		registerManualParts(50, Block.brick);
		registerManualParts(51, Block.tnt);
		registerManualParts(52, Block.bookShelf);
		registerManualParts(53, Block.cobblestoneMossy);
		registerManualParts(54, Block.obsidian);
		registerManualParts(55, Block.mobSpawner);
		registerManualParts(56, Block.oreDiamond);
		registerManualParts(57, Block.blockDiamond);
		registerManualParts(58, Block.workbench);
		registerManualParts(59, Block.furnaceIdle);
		registerManualParts(60, Block.oreRedstone);
		registerManualParts(61, Block.blockSnow);
		registerManualParts(62, Block.blockClay);
		registerManualParts(63, Block.jukebox);
		registerManualParts(64, Block.pumpkin);
		registerManualParts(65, Block.netherrack);
		registerManualParts(66, Block.slowSand);
		registerManualParts(67, Block.glowStone);
		registerManualParts(68, Block.pumpkinLantern);
		registerManualParts(69, Block.stoneBrick);
		registerManualParts(70, Block.melon);
		registerManualParts(71, Block.mycelium);
		registerManualParts(72, Block.netherBrick);
		registerManualParts(73, Block.whiteStone, 0);
		registerManualParts(74, Block.whiteStone, 1);
		registerManualParts(75, Block.oreEmerald);
		registerManualParts(76, Block.blockEmerald);
		registerManualParts(77, Block.commandBlock);
		registerManualParts(78, Block.sandStone, 1);
		registerManualParts(79, Block.sandStone, 2);
		registerManualParts(80, Block.redstoneLampIdle);
		registerManualParts(81, Block.stoneBrick, 1);
		registerManualParts(82, Block.stoneBrick, 2);
		registerManualParts(83, Block.stoneBrick, 3);
		registerManualParts(84, Block.blockRedstone);
		registerManualParts(85, Block.oreNetherQuartz);
		registerManualParts(86, Block.blockNetherQuartz, 0);
		registerManualParts(87, Block.blockNetherQuartz, 1);
		registerManualParts(88, Block.blockNetherQuartz, 2);
		registerManualParts(89, Block.dropper);
		registerManualParts(90, Block.stoneDoubleSlab, 0, Block.stoneSingleSlab, 0);
	}

	public void initializeBlockScan() {
		ArrayList<Integer> IDBlacklist = new ArrayList<Integer>();
		ArrayList<Integer> IDWhitelist = new ArrayList<Integer>();
		
		// TODO: add config option to manually add these
		IDBlacklist.add(Block.bedrock.blockID);
		IDBlacklist.add(Block.grass.blockID);
		IDBlacklist.add(Block.leaves.blockID);
		IDBlacklist.add(Block.sponge.blockID);
		IDBlacklist.add(Block.lockedChest.blockID);
		IDWhitelist.add(Block.glass.blockID);
		
		for (Block b : Block.blocksList) {
			if (b == null || b.blockID == 0) {
				continue;
			}
			if (IDBlacklist.contains(b.blockID)) {
				continue;
			}
			if ((!b.isOpaqueCube() || b.hasTileEntity(0) || !b.renderAsNormalBlock()) && !IDWhitelist.contains(b.blockID)) {
				continue;
			}

			ItemStack candidate = new ItemStack(b, 1);
			if (candidate.getHasSubtypes()) {
				Set<String> names = Sets.newHashSet();
				for (int meta = 0; meta < 16; meta++) {
					ItemStack is = new ItemStack(b, 1, meta);
					try {
						String itemName = this.getItemDisplayName(b.blockID, meta);
						if (!Strings.isNullOrEmpty(itemName) && names.add(itemName)) {
							this.addCuttableBlock(b, meta);
						}
					} catch (Throwable t) {
					}
					
				}
			} else {
				this.addCuttableBlock(b, 0);
			}
		}
	}

	private void registerManualParts(int n, Block block, int blockMeta) {
		registerManualParts(n, block, blockMeta, block, blockMeta);
	}

	private void registerManualParts(int n, Block block) {
		registerManualParts(n, block, 0);
	}

	private static class PartRegistrationType {
		public EnumPartClass clazz;
		public double size;
		public String prefix, suffix;

		public PartRegistrationType(EnumPartClass c, double s, String pr, String su) {
			clazz = c;
			size = s;
			prefix = pr;
			suffix = su;
		}
	}

	private static PartRegistrationType blockparts[] = new PartRegistrationType[] { new PartRegistrationType(EnumPartClass.Panel, 1.0 / 8.0, "", " Cover"), new PartRegistrationType(EnumPartClass.Panel, 2.0 / 8.0, "", " Panel"), new PartRegistrationType(EnumPartClass.Panel, 3.0 / 8.0, "", " Triple Cover"), new PartRegistrationType(EnumPartClass.Panel, 4.0 / 8.0, "", " Slab"), new PartRegistrationType(EnumPartClass.Panel, 5.0 / 8.0, "", " Cover Slab"), new PartRegistrationType(EnumPartClass.Panel, 6.0 / 8.0, "", " Triple Panel"), new PartRegistrationType(EnumPartClass.Panel, 7.0 / 8.0, "", " Anticover"), null, new PartRegistrationType(EnumPartClass.Strip, 1.0 / 8.0, "", " Cover Strip"), new PartRegistrationType(EnumPartClass.Strip, 2.0 / 8.0, "", " Panel Strip"), new PartRegistrationType(EnumPartClass.Strip, 3.0 / 8.0, "", " Triple Cover Strip"), new PartRegistrationType(EnumPartClass.Strip, 4.0 / 8.0, "", " Slab Strip"), new PartRegistrationType(EnumPartClass.Strip, 5.0 / 8.0, "", " Cover Slab Strip"), new PartRegistrationType(EnumPartClass.Strip, 6.0 / 8.0, "", " Triple Panel Strip"), new PartRegistrationType(EnumPartClass.Strip, 7.0 / 8.0, "", " Anticover Strip"), null, new PartRegistrationType(EnumPartClass.Corner, 1.0 / 8.0, "", " Cover Corner"), new PartRegistrationType(EnumPartClass.Corner, 2.0 / 8.0, "", " Panel Corner"), new PartRegistrationType(EnumPartClass.Corner, 3.0 / 8.0, "", " Triple Cover Corner"), new PartRegistrationType(EnumPartClass.Corner, 4.0 / 8.0, "", " Slab Corner"), new PartRegistrationType(EnumPartClass.Corner, 5.0 / 8.0, "", " Cover Slab Corner"), new PartRegistrationType(EnumPartClass.Corner, 6.0 / 8.0, "", " Triple Panel Corner"), new PartRegistrationType(EnumPartClass.Corner, 7.0 / 8.0, "", " Anticover Corner"), null, new PartRegistrationType(EnumPartClass.HollowPanel, 1.0 / 8.0, "Hollow ", " Cover"), new PartRegistrationType(EnumPartClass.HollowPanel, 2.0 / 8.0, "Hollow ", " Panel"), new PartRegistrationType(EnumPartClass.HollowPanel, 3.0 / 8.0, "Hollow ", " Triple Cover"), new PartRegistrationType(EnumPartClass.HollowPanel, 4.0 / 8.0, "Hollow ", " Slab"), new PartRegistrationType(EnumPartClass.HollowPanel, 5.0 / 8.0, "Hollow ", " Cover Slab"), new PartRegistrationType(EnumPartClass.HollowPanel, 6.0 / 8.0, "Hollow ", " Triple Panel"), new PartRegistrationType(EnumPartClass.HollowPanel, 7.0 / 8.0, "Hollow ", " Anticover"), null, };

	private void registerManualParts(int n, Block block, int meta, Block craftingBlock, int craftingMeta) {
		registerParts(n * 64, block, meta, craftingBlock, craftingMeta, false);
	}

	private static String getItemDisplayName(int itemID, int meta) {
		String nameKey = Item.itemsList[itemID].getUnlocalizedName(new ItemStack(itemID, 1, meta)) + ".name";
		String name = new ItemStack(itemID, 1, meta).getDisplayName();//StringTranslate.getInstance().translateKey(nameKey);

		if (name.equals(nameKey) || name.equals("")) {
			try {
				name = LanguageRegistry.instance().getStringLocalization(nameKey);
			}
			catch (Throwable t) {
				name = null;
			}
			if (name == null || name.equals(nameKey) || name.equals("")) {
				name = LanguageRegistry.instance().getStringLocalization(nameKey, "en_US");
				if (name == null || name.equals(nameKey) || name.equals("")) {
					try {
						name = Item.itemsList[itemID].getItemDisplayName(new ItemStack(itemID, 1, meta));
					} catch (Throwable t) {
						name = null;
					}

					if (name == null || name.equals(nameKey) || name.equals("")) {
						return null;
					}
				}
			}
		}
		return name;
	}

	private void registerParts(int partIDBase, Block block, int meta, Block craftingBlock, int craftingMeta, boolean ignoreNameCheck) {
		assert (blockparts.length == 32);

		String name = getItemDisplayName(craftingBlock.blockID, craftingMeta);
		if (name == null) {
			if (ignoreNameCheck) {
				name = "Unknown";
			} else
				return;
		}

		for (int k = 0; k < 7; k++) {
			// making hollow covers
			RecipeHollowCover.addMap(partIDBase + k, partIDBase + k + 24);
			// reverting hollow covers
			RecipeUnHollowCover.addMap(partIDBase + k + 24, partIDBase + k);

			// cutting panels into strips
			RecipeHorizontalCut.addMap(new BlockMetaPair(ProjectRed.blockMicrocontainer.blockID, partIDBase + k), ItemBlockMicroblock.getStackWithPartID(partIDBase + k + 8, 2));

			// cutting strips into corners
			RecipeHorizontalCut.addMap(new BlockMetaPair(ProjectRed.blockMicrocontainer.blockID, partIDBase + k + 8), ItemBlockMicroblock.getStackWithPartID(partIDBase + k + 16, 2));

			// combining corners into strips
			RecipeCombineTwo.addMap(partIDBase + k + 16, partIDBase + k + 8);

			// combining strips into panels
			RecipeCombineTwo.addMap(partIDBase + k + 8, partIDBase + k);
		}

		// combining multiple panels
		RecipeCombineSeveral.addMap(partIDBase, new ItemStack(craftingBlock, 1, craftingMeta));

		// combining multiple hollow panels
		RecipeCombineSeveral.addMap(partIDBase + 24, new ItemStack(craftingBlock, 1, craftingMeta));

		// cutting full blocks/slabs/panels
		RecipeVerticalCut.addMap(new BlockMetaPair(craftingBlock.blockID, craftingMeta), ItemBlockMicroblock.getStackWithPartID(partIDBase + 3, 2));
		RecipeVerticalCut.addMap(new BlockMetaPair(ProjectRed.blockMicrocontainer.blockID, partIDBase + 3), ItemBlockMicroblock.getStackWithPartID(partIDBase + 1, 2));
		RecipeVerticalCut.addMap(new BlockMetaPair(ProjectRed.blockMicrocontainer.blockID, partIDBase + 1), ItemBlockMicroblock.getStackWithPartID(partIDBase + 0, 2));

		// cutting hollow slabs/panels
		RecipeVerticalCut.addMap(new BlockMetaPair(ProjectRed.blockMicrocontainer.blockID, partIDBase + 27), ItemBlockMicroblock.getStackWithPartID(partIDBase + 25, 2));
		RecipeVerticalCut.addMap(new BlockMetaPair(ProjectRed.blockMicrocontainer.blockID, partIDBase + 25), ItemBlockMicroblock.getStackWithPartID(partIDBase + 24, 2));

		// Actually register the parts, and add them to the hashmap of id to
		// list of parts.
		ArrayList<Integer> subblocks = new ArrayList<Integer>();

		for (int k = 0; k < blockparts.length; k++)
			if (blockparts[k] != null) {
				String unlocalizedName = "projectred.microblocks." + (partIDBase + k);
				String localizedName = blockparts[k].prefix + name + blockparts[k].suffix;
				LanguageRegistry.instance().addStringLocalization(unlocalizedName + ".name", localizedName);

				PartType<Part> type = new DefaultPartType(partIDBase + k, blockparts[k].clazz, blockparts[k].size, unlocalizedName, block, meta);
				registerPartType(type);
			}
	}

	@Override
	public void registerPartType(PartType<?> type) {
		int id = type.getID();
		if (parts.containsKey(id)) {
			throw new RuntimeException("part id: " + id + " already in use when adding " + id + " with type " + parts.get(id));
		}
		parts.put(id, type);
		neiPartIDs.add(id);
	}

	@Override
	public IMicroblockCoverSystem createMicroblockCoverSystem(IMicroblockSupporterTile tile) {
		return new MicroblockCoverSystem(tile);
	}

	@Override
	public void addCuttableBlock(Block block, int meta) {
		if (block.blockID < 1 || block.blockID > 4095) {
			throw new IllegalArgumentException("BlockID must be between 1 and 4095 inclusive");
		}
		if (meta < 0 || meta > 1023) {
			throw new IllegalArgumentException("meta must be between 0 and 1023 inclusive");
		}
		registerParts(((block.blockID & 4095) << 20) | ((meta & 1023) << 10), block, meta, block, meta, true);
	}

	@Override
	public PartType<?> getPartTypeByID(int id) {
		return parts.get(id);
	}

	@Override
	public ItemStack partTypeIDToItemStack(int id, int stackSize) throws IllegalArgumentException {
		if (!parts.containsKey(id))
			throw new IllegalArgumentException("No part with ID " + id + " (hex: " + Integer.toHexString(id) + ")");
		return ItemBlockMicroblock.getStackWithPartID(id, stackSize);
	}

	@Override
	public int itemStackToPartID(ItemStack stack) throws NullPointerException, IllegalArgumentException {
		if (stack.itemID != ProjectRed.blockMicrocontainer.blockID)
			throw new IllegalArgumentException("Not a stack of microblocks");
		return ItemBlockMicroblock.getPartTypeID(stack);
	}

	public static synchronized IMicroblockLibrary getMicroblockSystem() {
		return instance;
	}

	/**
	 * If there is a microblock container block at the specified coordinates,
	 * this function will save the parts in that block, place a new block,
	 * restore the parts, and return true. If there is not a microblock
	 * container block at the specified coordinates, or placing the block fails,
	 * it will return false. The new block must have a tile entity which
	 * implements IMicroblockSupporterTile and has a non-null
	 * IMicroblockCoverSystem.
	 */
	public boolean mergeIntoMicroblockContainer(ItemStack itemstack, EntityPlayer entityplayer, World world, int x, int y, int z, int l, int newBlockID, int newMetadata) {
		if (ProjectRed.blockMicrocontainer == null || world.getBlockId(x, y, z) != ProjectRed.blockMicrocontainer.blockID)
			return false;

		IMicroblockSupporterTile tm = (IMicroblockSupporterTile) world.getBlockTileEntity(x, y, z);
		IMicroblockCoverSystem oldCI = tm.getCoverSystem();
		if (!world.setBlock(x, y, z, newBlockID, newMetadata, 2))
			return false;

		IMicroblockSupporterTile tcb = (IMicroblockSupporterTile) world.getBlockTileEntity(x, y, z);
		IMicroblockCoverSystem newCI = tcb.getCoverSystem();

		for (Part p : oldCI.getAllParts()) {
			newCI.addPart(p);
		}
		Block b = Block.blocksList[newBlockID];

		b.onBlockPlacedBy(world, x, y, z, entityplayer, itemstack);
		return true;
	}

	@Override
	public Block getMicroblockContainerBlock() {
		return ProjectRed.blockMicrocontainer;
	}

}
