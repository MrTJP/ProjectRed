package mrtjp.projectred.crafting;

import static codechicken.nei.api.API.addSetRange;

import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.blocks.BlockMachines.EnumMachine;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.integration.EnumGate;
import mrtjp.projectred.items.ItemBackpack.EnumBackpack;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.multipart.microblocks.ItemBlockMicroblock;
import mrtjp.projectred.multipart.microblocks.MicroblockLibrary;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import net.minecraft.item.ItemStack;
import codechicken.nei.MultiItemRange;
import codechicken.nei.api.API;
import codechicken.nei.api.IConfigureNEI;

public class NEIProjectRedConfig implements IConfigureNEI {
	@Override
	public void loadConfig() {
		try {
			// Microblock section
			API.setMaxDamageException(ProjectRed.blockMicrocontainer.blockID, 1);
			for (int i : MicroblockLibrary.neiPartIDs) {
				API.addNBTItem(ItemBlockMicroblock.getStackWithPartID(i));
			}
			addSetRange("ProjectRed.Microblocks", new MultiItemRange().add(ProjectRed.blockMicrocontainer.blockID));

			// Wiring
			MultiItemRange wiring = new MultiItemRange();
			for (EnumWire w : EnumWire.VALID_WIRE) {
				wiring.add(w.getItemStack());
			}
			addSetRange("ProjectRed.Wiring", wiring);

			// Gates
			MultiItemRange gates = new MultiItemRange();
			for (EnumGate g : EnumGate.VALUES) {
				gates.add(g.getItemStack());
			}
			addSetRange("ProjectRed.Gates", gates);

			// Lighting
			MultiItemRange lighting = new MultiItemRange();
			lighting.add(ProjectRed.blockLamp, 0, EnumLamp.VALID_TYPES.length - 1);
			for (EnumLamp l : EnumLamp.VALID_TYPES) {
				lighting.add(l.getItemStack());
			}
			for (EnumLamp l : EnumLamp.VALID_TYPES) {
				lighting.add(l.getInvertedItemStack());
			}
			for (EnumPart p : EnumPart.ILLUMAR_PARTS) {
				lighting.add(p.getItemStack());
			}
			addSetRange("ProjectRed.Lighting", lighting);

			// Machines
			MultiItemRange machines = new MultiItemRange();
			for (EnumMachine m : EnumMachine.VALID_MACHINES) {
				machines.add(m.getItemStack());
			}
			addSetRange("ProjectRed.Machines", machines);

			// Parts
			MultiItemRange parts = new MultiItemRange();
			for (EnumPart p : EnumPart.VALID_PARTS) {
				if (!(Arrays.asList(EnumPart.ILLUMAR_PARTS).contains(p))) {
					parts.add(p.getItemStack());
				}
			}
			addSetRange("ProjectRed.Parts", parts);

			// Tools
			MultiItemRange tools = new MultiItemRange();
			tools.add(new ItemStack(ProjectRed.itemSaw));
			tools.add(new ItemStack(ProjectRed.itemScrewdriver));
			tools.add(new ItemStack(ProjectRed.itemDrawPlate));
			tools.add(new ItemStack(ProjectRed.itemWoolGin));
			for (EnumBackpack b : EnumBackpack.VALID_BP) {
				tools.add(b.getItemStack());
			}
			addSetRange("ProjectRed.Tools", tools);

			API.registerRecipeHandler(new NEIAlloySmelterRecipeManager());
			API.registerUsageHandler(new NEIAlloySmelterRecipeManager());

		} catch (Throwable e) {
			e.printStackTrace();
		}
	}

	@Override
	public String getName() {
		return "Project Red";
	}

	@Override
	public String getVersion() {
		return Configurator.version;
	}
}
