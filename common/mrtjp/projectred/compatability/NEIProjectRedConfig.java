package mrtjp.projectred.compatability;

import mrtjp.projectred.core.Configurator;
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
