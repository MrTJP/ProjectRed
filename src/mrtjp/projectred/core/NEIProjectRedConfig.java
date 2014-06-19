package mrtjp.projectred.core;

import codechicken.nei.api.IConfigureNEI;
import mrtjp.projectred.core.libmc.recipe.RecipeLib;

public class NEIProjectRedConfig implements IConfigureNEI
{
    @Override
    public void loadConfig()
    {
        try
        {
            RecipeLib.loadNEI();
        }
        catch (Throwable e)
        {
            e.printStackTrace();
        }
    }

    @Override
    public String getName()
    {
        return "Project Red";
    }

    @Override
    public String getVersion()
    {
        return Configurator.version + "." + Configurator.buildnumber;
    }
}
