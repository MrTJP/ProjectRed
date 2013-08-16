package mrtjp.projectred.core;

import static codechicken.nei.api.API.addSetRange;

import java.util.Arrays;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import mrtjp.projectred.exploration.ItemBackpack.EnumBackpack;
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
        return Configurator.version + "." + Configurator.buildnumber;
    }
}
