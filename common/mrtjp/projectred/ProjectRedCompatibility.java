package mrtjp.projectred;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.network.NetworkMod;

@Mod(modid = "ProjRed|Compatibility", useMetadata = true, dependencies = "after:ProjRed|Core")
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
public class ProjectRedCompatibility
{
    @Instance("ProjRed|Compatibility")
    public static ProjectRedCompatibility instance;

    @SidedProxy(clientSide = "mrtjp.projectred.compatibility.CompatibilityClientProxy", serverSide = "mrtjp.projectred.compatibility.CompatibilityProxy")
    public static IProxy proxy;

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event)
    {
        proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event)
    {
        MinecraftForge.EVENT_BUS.register(instance);
        proxy.init();
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event)
    {
        proxy.postinit();
    }
}
