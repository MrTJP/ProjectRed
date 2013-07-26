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
import mrtjp.projectred.items.ItemBackpack.EnumBackpack;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.multipart.wiring.gates.EnumGate;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import mrtjp.projectred.utils.Color;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;
import cpw.mods.fml.common.registry.GameRegistry;

public class CraftingRecipeManager {
	
	public static void initRecipes() {
		initGateRecipes();
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
					'W', new ItemStack(Block.cloth, 1, Color.get(i).woolId()),
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
					Color.get(bundledColor).getOreDict(),
					EnumWire.oreDictDefinitionBundled, 
					EnumWire.oreDictDefinitionBundled, 
					EnumWire.oreDictDefinitionBundled, 
					Color.get(bundledColor).getOreDict()
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
	private static void initGateRecipes() {

		/** AND Gate **/
		GameRegistry.addRecipe(EnumGate.AND.getItemStack(), 
				"ACA",
				"CCC",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),  
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** OR Gate **/
		GameRegistry.addRecipe(EnumGate.OR.getItemStack(), 
				"PCP",
				"WCW",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);

		/** NOT Gate**/
		GameRegistry.addRecipe(EnumGate.NOT.getItemStack(),
				"PCP",
				"CAC",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		
		/** RS Latch **/
		GameRegistry.addRecipe(EnumGate.RSLATCH.getItemStack(),
				"WWA",
				"CPC",
				"AWW",
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);

		/** Toggle Latch **/
		GameRegistry.addRecipe(EnumGate.TOGGLE.getItemStack(),
				"CPP",
				"WLW",
				"CPP",
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'L', Block.lever
		);

		/** NOR Gate **/
		GameRegistry.addRecipe(EnumGate.NOR.getItemStack(),
				"PAP",
				"WCW",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack()
		);
		
		/** NAND Gate **/
		GameRegistry.addRecipe(EnumGate.NAND.getItemStack(),
				"AAA",
				"CCC",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);

		/** XOR Gate **/
		GameRegistry.addRecipe(EnumGate.XOR.getItemStack(),
				"AWA",
				"CAC",
				"WCW",
				'A', EnumPart.ANODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack()
		);

		/** XNOR Gate **/
		GameRegistry.addRecipe(EnumGate.XNOR.getItemStack(),
				"ACA",
				"CAC",
				"WCW",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Buffer Gate **/
		GameRegistry.addRecipe(EnumGate.Buffer.getItemStack(),
				"ACA",
				"WCW",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);
		
		/** Multiplexer Gate **/
		GameRegistry.addRecipe(EnumGate.Multiplexer.getItemStack(),
				"ACA",
				"CPC",
				"ACW",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Repeater Gate **/
		GameRegistry.addRecipe(EnumGate.Repeater.getItemStack(),
				"PCA",
				"ACP",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Timer Gate **/
		GameRegistry.addRecipe(EnumGate.Timer.getItemStack(),
				"ACA",
				"WTW",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'T', EnumPart.POINTER.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);
		
		/** Counter Gate **/
		GameRegistry.addRecipe(EnumGate.Counter.getItemStack(),
				"PWP",
				"CTC",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'T', EnumPart.POINTER.getItemStack(),
				'A', EnumPart.ANODE.getItemStack()
		);
		
		/** Sequencer Gate **/
		GameRegistry.addRecipe(EnumGate.Sequencer.getItemStack(),
				"PCP",
				"CTC",
				"PCP",
				'P', EnumPart.PLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'T', EnumPart.POINTER.getItemStack()
		);
		
		/** Pulse Former Gate **/
		GameRegistry.addRecipe(EnumGate.PulseFormer.getItemStack(),
				"ACA",
				"CAC",
				"WWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);
		
		/** Randomizer Gate **/
		GameRegistry.addRecipe(EnumGate.Randomizer.getItemStack(),
				"PEP",
				"WWW",
				"EWE",
				'P', EnumPart.PLATE.getItemStack(),
				'E', EnumPart.ENERGIZEDSILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** State Cell Gate **/
		GameRegistry.addRecipe(EnumGate.StateCell.getItemStack(),
				"PAC",
				"WST",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'T', EnumPart.POINTER.getItemStack()
		);
		
		/** Synchronizer Gate **/
		GameRegistry.addRecipe(EnumGate.Synchronizer.getItemStack(),
				"WCW",
				"SAS",
				"WWW",
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'A', EnumPart.ANODE.getItemStack()
		);
		
		/** D-Latch Gate **/
		GameRegistry.addRecipe(EnumGate.DLatch.getItemStack(),
				"PAP",
				"ASW",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** D-FlipFlop Gate **/
		GameRegistry.addRecipe(EnumGate.DFlop.getItemStack(),
				"PAP",
				"ASA",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Bundled Latch **/
		GameRegistry.addRecipe(EnumGate.BundledLatch.getItemStack(),
				"PBP",
				"GSW",
				"PBP",
				'P', EnumPart.PLATE.getItemStack(),
				'B', EnumPart.BUNDLEDPLATE.getItemStack(),
				'G', EnumGate.DLatch.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Bundled Relay **/
		GameRegistry.addRecipe(EnumGate.BundledRelay.getItemStack(),
				"PBP",
				"GSW",
				"PBP",
				'P', EnumPart.PLATE.getItemStack(),
				'B', EnumPart.BUNDLEDPLATE.getItemStack(),
				'G', EnumGate.Repeater.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Bundled Multiplexer **/
		GameRegistry.addRecipe(EnumGate.BundledMultiplexer.getItemStack(),
				"PBP",
				"BSB",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'B', EnumPart.BUNDLEDPLATE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Light Sensor **/
		GameRegistry.addRecipe(EnumGate.LightSensor.getItemStack(),
				"PPP",
				"LLL",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'L', new ItemStack(Item.dyePowder, 1, Color.BLUE.dyeId()),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);

		/** Rain Sensor **/
		GameRegistry.addRecipe(EnumGate.RainSensor.getItemStack(),
				"PPP",
				"SSS",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'S', Item.slimeBall,
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);

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
					Color.get(i).getOreDict(),
					Color.get(i).getOreDict()
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
		
		/** Screw Driver **/
		GameRegistry.addRecipe(new ItemStack(ProjectRed.itemScrewdriver),
				"i  ",
				" ib",
				" bi",
				'i', Item.ingotIron,
				'b', new ItemStack(Item.dyePowder, 1, Color.BLUE.dyeId())
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
					'd', Color.get(i).getOreDict()
			));
		}
		GameRegistry.addRecipe(new RecipeBackpackRecoloring());
		
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

