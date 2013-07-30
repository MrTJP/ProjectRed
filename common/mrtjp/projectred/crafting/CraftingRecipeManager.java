package mrtjp.projectred.crafting;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.blocks.BlockMachines.EnumMachine;
import mrtjp.projectred.crafting.microblocks.RecipeCombineSeveral;
import mrtjp.projectred.crafting.microblocks.RecipeCombineTwo;
import mrtjp.projectred.crafting.microblocks.RecipeHollowCover;
import mrtjp.projectred.crafting.microblocks.RecipeHorizontalCut;
import mrtjp.projectred.crafting.microblocks.RecipeUnHollowCover;
import mrtjp.projectred.crafting.microblocks.RecipeVerticalCut;
import mrtjp.projectred.crafting.tools.RecipeBackpackRecoloring;
import mrtjp.projectred.crafting.tools.RecipeDrawPlate;
import mrtjp.projectred.crafting.tools.RecipeVAWTRecoloring;
import mrtjp.projectred.integration.EnumGate;
import mrtjp.projectred.items.ItemBackpack.EnumBackpack;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import mrtjp.projectred.utils.PRColors;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;
import cpw.mods.fml.common.registry.GameRegistry;

public class CraftingRecipeManager {
	
	public static void initRecipes() {
		initMicroblockRecipes();
		initWireRecipes();
		initPartsRecipes();
		initMachineRecipes();
		initToolRecipes();
		initOtherRecipes();
		initOtherAlloySmelterRecipes();
	}
	
	private static void initMicroblockRecipes() {
		GameRegistry.addRecipe(new RecipeHollowCover());
		GameRegistry.addRecipe(new RecipeUnHollowCover());
		GameRegistry.addRecipe(new RecipeVerticalCut());
		GameRegistry.addRecipe(new RecipeHorizontalCut());
		GameRegistry.addRecipe(new RecipeCombineTwo());
		GameRegistry.addRecipe(new RecipeCombineSeveral());
	}
	private static void initWireRecipes() {	
		/** Red Alloy Wires **/
		GameRegistry.addRecipe(EnumWire.RED_ALLOY.getItemStack(12), 
				" r ",
				" r ",
				" r ",
				'r', EnumPart.REDINGOT.getItemStack()
		);
		
		/** Insulated Wires **/
		for (int i = 0; i < EnumWire.INSULATED_WIRE.length; i++) {
			EnumWire w = EnumWire.INSULATED_WIRE[i];
			GameRegistry.addRecipe(w.getItemStack(12), 
					"WrW",
					"WrW",
					"WrW",
					'W', new ItemStack(Block.cloth, 1, PRColors.get(i).woolId()),
					'r', EnumPart.REDINGOT.getItemStack()
			);
		}
		
		/** Bundled Cables **/
		GameRegistry.addRecipe(new ShapedOreRecipe(EnumWire.BUNDLED_N.getItemStack(), 
				"SWS",
				"WWW",
				"SWS",
				'S', Item.silk,
				'W', EnumWire.oreDictDefinitionInsulated
		));
		int bundledColor = 0;
		for (EnumWire w : EnumWire.BUNDLED_WIRE) {
			if (w == EnumWire.BUNDLED_N) {
				continue;
			}
			GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(3),
					PRColors.get(bundledColor).getOreDict(),
					EnumWire.oreDictDefinitionBundled, 
					EnumWire.oreDictDefinitionBundled, 
					EnumWire.oreDictDefinitionBundled, 
					PRColors.get(bundledColor).getOreDict()
			));
			bundledColor++;
		}
		
		/** Jacketed Wiring **/
		for (EnumWire w : EnumWire.VALID_WIRE) {
			if (w.hasJacketedForm()) {
				// Regular to jacketed
				GameRegistry.addRecipe(w.getJacketedItemStack(3), 
						"sis",
						"sis",
						"sis",
						'i', w.getItemStack(),
						's', Item.stick
				);
				// Jacketed to regular
				GameRegistry.addRecipe(w.getItemStack(3), 
						"i",
						"i",
						"i",
						'i', w.getJacketedItemStack()
				);
			}
		}
		
	}
	
	private static void initPartsRecipes() {
		/** Circuit Plate **/
		GameRegistry.addSmelting(Block.stone.blockID, EnumPart.PLATE.getItemStack(2), 0f);
		
		/** Conductive Plate **/
		GameRegistry.addRecipe(EnumPart.CONDUCTIVEPLATE.getItemStack(), 
				"r",
				"p",
				'r', Item.redstone,
				'p', EnumPart.PLATE.getItemStack()
		);
		
		/** Wired Plate **/
		GameRegistry.addRecipe(EnumPart.WIREDPLATE.getItemStack(), 
				"r",
				"p",
				'r', EnumWire.RED_ALLOY.getItemStack(),
				'p', EnumPart.PLATE.getItemStack()
		);

		/** Bundled Plate **/
		GameRegistry.addRecipe(EnumPart.BUNDLEDPLATE.getItemStack(), 
				"r",
				"p",
				'r', EnumWire.BUNDLED_N.getItemStack(),
				'p', EnumPart.PLATE.getItemStack()
		);
		
		/** Anode **/
		GameRegistry.addRecipe(EnumPart.ANODE.getItemStack(3), 
				" r ",
				"rrr",
				"ppp",
				'r', Item.redstone,
				'p', EnumPart.PLATE.getItemStack()
		);
		
		/** Cathode **/
		GameRegistry.addRecipe(EnumPart.CATHODE.getItemStack(), 
				"t",
				"p",
				't', Block.torchRedstoneActive,
				'p', EnumPart.PLATE.getItemStack()
		);
		
		/** Pointer **/
		GameRegistry.addRecipe(EnumPart.POINTER.getItemStack(), 
				"b",
				"m",
				"c",
				'b', Block.stone,
				'm', EnumPart.MOTOR.getItemStack(),
				'c', EnumPart.CATHODE.getItemStack()
		);

		/** Silicon Chip **/
		GameRegistry.addRecipe(EnumPart.SILICONCHIP.getItemStack(), 
				" s ",
				"ppp",
				's', EnumPart.INFUSEDSILICON.getItemStack(),
				'p', EnumPart.PLATE.getItemStack()
		);
		
		/** Energized Silicon Chip **/
		GameRegistry.addRecipe(EnumPart.ENERGIZEDSILICONCHIP.getItemStack(), 
				" e ",
				"ppp",
				'e', EnumPart.ENERGIZEDSILICON.getItemStack(),
				'p', EnumPart.PLATE.getItemStack()
		);
		
		/** Platformed Plate **/
		GameRegistry.addRecipe(EnumPart.PLATFORMEDPLATE.getItemStack(), 
				" r ",
				"sps",
				"prp",
				'r', EnumPart.WIREDPLATE.getItemStack(),
				's', Item.stick,
				'p', EnumPart.PLATE.getItemStack()
		);

		
		/** Silicon Boule **/
		AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
				new ItemStack(Block.sand, 8),
				new ItemStack(Item.coal, 8),
		}, EnumPart.SILICONBOULE.getItemStack(), 500));
		
		/** Silicon **/
		GameRegistry.addRecipe(EnumPart.SILICON.getItemStack(8), 
				"s",
				"b",
				's', new ItemStack(ProjectRed.itemSaw, 1, Short.MAX_VALUE), 
				'b', EnumPart.SILICONBOULE.getItemStack()
		);
		
		/** Infused Silicon **/
		AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
				EnumPart.SILICON.getItemStack(),
				new ItemStack(Item.redstone, 4),
		}, EnumPart.INFUSEDSILICON.getItemStack(), 150));
		
		/** Energized Silicon **/
		AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
				EnumPart.SILICON.getItemStack(),
				new ItemStack(Item.glowstone, 4),
		}, EnumPart.ENERGIZEDSILICON.getItemStack(), 160));
		
		/** Motor **/
		GameRegistry.addRecipe(EnumPart.MOTOR.getItemStack(), 
				" i ",
				"scs",
				"rcr",
				'i', Item.ingotIron,
				's', Block.stone,
				'c', EnumPart.COPPERCOIL.getItemStack(),
				'r', Item.redstone
		);
		
		/** Copper Coil **/
		GameRegistry.addRecipe(new ShapedOreRecipe(EnumPart.COPPERCOIL.getItemStack(), 
				"cd",
				'c', "ingotCopper",
				'd', new ItemStack(ProjectRed.itemDrawPlate, 1, Short.MAX_VALUE)
		));
		
		/** Iron Coil **/
		GameRegistry.addRecipe(EnumPart.IRONCOIL.getItemStack(), 
				"cd",
				'c', new ItemStack(Item.ingotIron),
				'd', new ItemStack(ProjectRed.itemDrawPlate, 1, Short.MAX_VALUE)
		);
		
		/** Gold Coil **/
		GameRegistry.addRecipe(EnumPart.GOLDCOIL.getItemStack(), 
				"cd",
				'c', new ItemStack(Item.ingotGold),
				'd', new ItemStack(ProjectRed.itemDrawPlate, 1, Short.MAX_VALUE)
		);
		
		/** Red Alloy Ingot **/
		AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
				new ItemStack(Item.ingotIron),
				new ItemStack(Item.redstone, 4),
		}, EnumPart.REDINGOT.getItemStack(), 125));
		
		/** Illumar **/
		for (int i = 0; i < EnumPart.ILLUMAR_PARTS.length; i++) {
			EnumPart p = EnumPart.ILLUMAR_PARTS[i];
			GameRegistry.addRecipe(new ShapelessOreRecipe(p.getItemStack(), 
					new ItemStack(Item.glowstone),
					new ItemStack(Item.glowstone),
					PRColors.get(i).getOreDict(),
					PRColors.get(i).getOreDict()
			));
		}
		
		/** Woven Cloth **/
		GameRegistry.addRecipe(EnumPart.WOVENCLOTH.getItemStack(), 
				"sss",
				"sws",
				"sss",
				's', Item.silk,
				'w', Item.stick
		);
		
		/** Sail **/
		GameRegistry.addRecipe(EnumPart.WOVENCLOTH.getItemStack(), 
				"ss",
				"ss",
				"ss",
				's', EnumPart.SAIL.getItemStack()
		);
	}
	private static void initMachineRecipes() {
		
		/** Lamps **/
		for (EnumLamp l : EnumLamp.VALID_TYPES) {
			GameRegistry.addRecipe(l.getItemStack(),  // Regular
					"gIg",
					"gIg",
					"gtg",
					'g', Block.thinGlass,
					'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
					't', Item.redstone
			);
			GameRegistry.addRecipe(l.getInvertedItemStack(), // Inverted
					"gIg",
					"gIg",
					"gtg",
					'g', Block.thinGlass,
					'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
					't', Block.torchRedstoneActive
			);
		}
		
		/** Alloy Smelter **/
		GameRegistry.addRecipe(EnumMachine.ALLOYSMELTER.getItemStack(), 
				"CBC",
				"BBB",
				"CBC",
				'C', Block.blockClay,
				'B', Block.brick
		);
		
		/** Lanterns **/
		for (EnumLantern l : EnumLantern.VALID_TYPES) {
			GameRegistry.addRecipe(l.getItemStack(), 
					"PNP",
					"GIG",
					"PRP",
					'P', EnumPart.PLATE.getItemStack(),
					'N', Item.goldNugget,
					'G', Block.thinGlass,
					'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
					'R', Item.redstone	
			);
			GameRegistry.addRecipe(l.getInvertedItemStack(), 
					"PNP",
					"GIG",
					"PRP",
					'P', EnumPart.PLATE.getItemStack(),
					'N', Item.goldNugget,
					'G', Block.thinGlass,
					'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
					'R', Block.torchRedstoneActive
			);
		}
	}	
	private static void initToolRecipes() {
		/** Saw **/
		GameRegistry.addRecipe(new ItemStack(ProjectRed.itemSaw), 
				"sss",
				"ii ",
				"dd ",
				's', Item.stick,
				'i', Item.ingotIron,
				'd', Item.diamond
		);
		
		/** Draw Plate **/
		GameRegistry.addRecipe(new RecipeDrawPlate());
		
		/** Wool Gin **/
		GameRegistry.addRecipe(new ItemStack(ProjectRed.itemWoolGin), 
				"sis",
				"sss",
				" s ",
				's', Item.stick,
				'i', EnumPart.IRONCOIL.getItemStack()
		);
		
		/** Backpacks **/
		for (int i = 0; i < EnumBackpack.VALID_BP.length; i++) {
			GameRegistry.addRecipe(new ShapedOreRecipe(EnumBackpack.get(i).getItemStack(), 
					"ccc",
					"cdc",
					"ccc",
					'c', EnumPart.WOVENCLOTH.getItemStack(),
					'd', PRColors.get(i).getOreDict()
			));
		}
		GameRegistry.addRecipe(new RecipeBackpackRecoloring());
		
		/** VAWT **/
		GameRegistry.addRecipe(new ItemStack(ProjectRed.itemVAWT, 1), 
				"sss",
				"ttt",
				"sss",
				's', EnumPart.SAIL.getItemStack(),
				't', Item.stick
		);
		GameRegistry.addRecipe(new RecipeVAWTRecoloring());
	}
	private static void initOtherRecipes() {
		/** Wool Gin to string recipe **/
		GameRegistry.addRecipe(new ItemStack(Item.silk, 4), 
				"gw",
				'g', new ItemStack(ProjectRed.itemWoolGin, 1, Short.MAX_VALUE),
				'w', Block.cloth
		);
	}
	private static void initOtherAlloySmelterRecipes() {
		/** Red Alloy Ingot reset recipes **/
		AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
				EnumWire.RED_ALLOY.getItemStack(4),
		}, EnumPart.REDINGOT.getItemStack(), 50));
		AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
				EnumWire.BUNDLED_N.getItemStack(8),
		}, EnumPart.REDINGOT.getItemStack(5), 90));
		for (EnumWire w : EnumWire.INSULATED_WIRE) {
			AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
					w.getItemStack(4)
			}, EnumPart.REDINGOT.getItemStack(), 80));
		}
	}


}

