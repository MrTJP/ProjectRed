package mrtjp.projectred.core;

import codechicken.nei.api.API;
import codechicken.nei.api.IConfigureNEI;

public class NEIProjectRedConfig implements IConfigureNEI {
    @Override
    public void loadConfig() {
        try {
            
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
