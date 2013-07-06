package mrtjp.projectred.crafting;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.blocks.BlockMachines.EnumMachine;
import mrtjp.projectred.items.ItemPart.EnumPart;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ProjectRedTabs {

	static {
		LanguageRegistry.instance().addStringLocalization("itemGroup.Project Red: Wiring", "en_US", "Project Red: Wiring");
		LanguageRegistry.instance().addStringLocalization("itemGroup.Project Red: Tools", "en_US", "Project Red: Tools");
		LanguageRegistry.instance().addStringLocalization("itemGroup.Project Red: Lighting", "en_US", "Project Red: Lighting");
		LanguageRegistry.instance().addStringLocalization("itemGroup.Project Red: Misc", "en_US", "Project Red: Misc");
		LanguageRegistry.instance().addStringLocalization("itemGroup.Project Red: Machines", "en_US", "Project Red: Machines");
	}

	public static CreativeTabs tabWires = new CreativeTabs("Project Red: Wiring") {
		@Override
		public ItemStack getIconItemStack() {
			return new ItemStack(ProjectRed.blockWire);
		}
	};
	
	public static CreativeTabs tabTools = new CreativeTabs("Project Red: Tools") {
		@Override
		public ItemStack getIconItemStack() {
			return new ItemStack(ProjectRed.itemSaw);
		}
	};
	
	public static CreativeTabs tabLighting = new CreativeTabs("Project Red: Lighting") {
		@Override
		public ItemStack getIconItemStack() {
			return EnumLamp.RED.getInvertedItemStack();
		}
	};
	
	public static CreativeTabs tabParts = new CreativeTabs("Project Red: Misc") {
		@Override
		public ItemStack getIconItemStack() {
			return EnumPart.REDINGOT.getItemStack();
		}
	};
	
	public static CreativeTabs tabMachines = new CreativeTabs("Project Red: Machines") {
		@Override
		public ItemStack getIconItemStack() {
			return EnumMachine.ALLOYSMELTER.getItemStack();
		}
	};




}
