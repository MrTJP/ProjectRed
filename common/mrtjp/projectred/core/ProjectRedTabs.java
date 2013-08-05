package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import mrtjp.projectred.illumination.EnumLantern;
import mrtjp.projectred.integration.EnumGate;
import net.minecraft.block.Block;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ProjectRedTabs {

	static {
		LanguageRegistry.instance().addStringLocalization("itemGroup.core", "en_US", "Project Red: Core");
		LanguageRegistry.instance().addStringLocalization("itemGroup.trans", "en_US", "Project Red: Transmission");
		LanguageRegistry.instance().addStringLocalization("itemGroup.int", "en_US", "Project Red: Integration");
		LanguageRegistry.instance().addStringLocalization("itemGroup.ill", "en_US", "Project Red: Illumination");
		LanguageRegistry.instance().addStringLocalization("itemGroup.expansion", "en_US", "Project Red: Expansion");
		LanguageRegistry.instance().addStringLocalization("itemGroup.exploration", "en_US", "Project Red: Exploration");
	}

	public static CreativeTabs tabCore = new CreativeTabs("core") {
		@Override
		public ItemStack getIconItemStack() {
			return new ItemStack(ProjectRed.itemScrewdriver);
		}
	};
	
	public static CreativeTabs tabTransmission = new CreativeTabs("trans") {
		@Override
		public ItemStack getIconItemStack() {
			return new ItemStack(ProjectRed.itemPartWire);
		}
	};
		
	public static CreativeTabs tabIntegration = new CreativeTabs("int") {
		@Override
		public ItemStack getIconItemStack() {
			return EnumGate.Timer.getItemStack();
		}
	};
	
	public static CreativeTabs tabLighting = new CreativeTabs("ill") {
		@Override
		public ItemStack getIconItemStack() {
			return EnumLantern.RED.getInvertedItemStack();
		}
	};
	
	public static CreativeTabs tabExpansion = new CreativeTabs("expansion") {
		@Override
		public ItemStack getIconItemStack() {
			return EnumMachine.ALLOYSMELTER.getItemStack();
		}
	};
	
	public static CreativeTabs tabExploration = new CreativeTabs("exploration") {
		@Override
		public ItemStack getIconItemStack() {
			return new ItemStack(Block.grass);
		}
	};

}
