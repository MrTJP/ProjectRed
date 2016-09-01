package mrtjp.projectred.core;

import codechicken.nei.api.API;
import codechicken.nei.api.IConfigureNEI;
import codechicken.nei.recipe.DefaultOverlayHandler;
import codechicken.nei.recipe.ICraftingHandler;
import codechicken.nei.recipe.IRecipeHandler;
import codechicken.nei.recipe.IUsageHandler;
import cpw.mods.fml.common.Loader;
import mrtjp.projectred.core.libmc.recipe.PRShapedRecipeHandler;
import mrtjp.projectred.core.libmc.recipe.PRShapelessRecipeHandler;
import mrtjp.projectred.expansion.GuiProjectBench;

public class NEIProjectRedConfig implements IConfigureNEI
{
    @Override
    public void loadConfig()
    {
        try
        {
            reg(new PRShapedRecipeHandler());
            reg(new PRShapelessRecipeHandler());
        }
        catch (Throwable e)
        {
            e.printStackTrace();
        }
    }

    private void reg(IRecipeHandler h)
    {
        API.registerUsageHandler((IUsageHandler)h);
        API.registerRecipeHandler((ICraftingHandler)h);
        if(Loader.isModLoaded("ProjRed|Exploration"))
        {
        	API.registerGuiOverlay(GuiProjectBench.class, "crafting", 5, 12);
            API.registerGuiOverlayHandler(GuiProjectBench.class, new DefaultOverlayHandler(23, 12), "crafting");
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
        return Configurator.version() + "." + Configurator.buildnumber();
    }
}
