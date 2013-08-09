package mrtjp.projectred.core;
import static codechicken.nei.api.API.addSetRange;

import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.ItemBackpack.EnumBackpack;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import mrtjp.projectred.illumination.EnumLamp;
import mrtjp.projectred.illumination.EnumLantern;
import mrtjp.projectred.integration.EnumGate;
import mrtjp.projectred.transmission.EnumWire;
import net.minecraft.item.ItemStack;
import codechicken.nei.MultiItemRange;
import codechicken.nei.api.API;
import codechicken.nei.api.IConfigureNEI;

public class NEIProjectRedConfig implements IConfigureNEI {
    @Override
    public void loadConfig() {
        try {

            // Wiring
            MultiItemRange wiring = new MultiItemRange();
            for (EnumWire w : EnumWire.VALID_WIRE) {
                wiring.add(w.getItemStack());
            }
            addSetRange("ProjectRed.Transmission", wiring);

            // Gates
            MultiItemRange gates = new MultiItemRange();
            for (EnumGate g : EnumGate.VALID_GATES) {
                gates.add(g.getItemStack());
            }
            addSetRange("ProjectRed.Integration", gates);

            // Lighting
            MultiItemRange lighting = new MultiItemRange();
            for (EnumLamp l : EnumLamp.VALID_TYPES) {
                lighting.add(l.getItemStack());
                lighting.add(l.getInvertedItemStack());
            }
            for (EnumLantern l : EnumLantern.VALID_TYPES) {
                lighting.add(l.getItemStack());
                lighting.add(l.getInvertedItemStack());
            }
            for (EnumPart p : EnumPart.ILLUMAR_PARTS) {
                lighting.add(p.getItemStack());
            }
            addSetRange("ProjectRed.Illumination", lighting);

            // Machines
            MultiItemRange machines = new MultiItemRange();
            for (EnumMachine m : EnumMachine.VALID_MACHINES) {
                machines.add(m.getItemStack());
            }
            addSetRange("ProjectRed.Expansion", machines);

            // Core
            MultiItemRange core = new MultiItemRange();
            for (EnumPart p : EnumPart.VALID_PARTS) {
                if (!(Arrays.asList(EnumPart.ILLUMAR_PARTS).contains(p))) {
                    core.add(p.getItemStack());
                }
                core.add(new ItemStack(ProjectRed.itemScrewdriver));
                core.add(new ItemStack(ProjectRed.itemDrawPlate));
                core.add(new ItemStack(ProjectRed.itemWoolGin));
                for (EnumBackpack b : EnumBackpack.VALID_BP) {
                    core.add(b.getItemStack());
                }
            }
            addSetRange("ProjectRed.Core", core);

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
