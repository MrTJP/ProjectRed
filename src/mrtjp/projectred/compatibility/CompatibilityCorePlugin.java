package mrtjp.projectred.compatibility;

import cpw.mods.fml.relauncher.IFMLLoadingPlugin;

import java.util.Map;

public class CompatibilityCorePlugin implements IFMLLoadingPlugin
{
    @Override
    public String[] getASMTransformerClass()
    {
        return new String[]{};
    }

    @Override
    public String getModContainerClass()
    {
        return null;
    }

    @Override
    public String getSetupClass()
    {
        return null;
    }

    @Override
    public void injectData(Map<String, Object> data)
    {
    }

    //kept for forge 953 compilation
    @Deprecated
    public String[] getLibraryRequestClass()
    {
        return null;
    }

    @Override
    public String getAccessTransformerClass() {
        return null;
    }
}
