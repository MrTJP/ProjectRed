package mrtjp.projectred.crafting;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.blocks.BlockMachines.EnumMachine;
import mrtjp.projectred.crafting.tools.RecipeVAWTRecoloring;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.transmission.EnumWire;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.GameRegistry;

public class CraftingRecipeManager {
	
	public static void initRecipes() {
		initMachineRecipes();
		initToolRecipes();
		initOtherAlloySmelterRecipes();
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

