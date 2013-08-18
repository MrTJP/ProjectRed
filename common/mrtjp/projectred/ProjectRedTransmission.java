package mrtjp.projectred;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.integration.ItemPartGate;
import mrtjp.projectred.transmission.ItemPartFramedWire;
import mrtjp.projectred.transmission.ItemPartWire;
import mrtjp.projectred.transmission.ItemWireDebugger;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.network.NetworkMod;

@Mod(modid = "ProjRed|Transmission", name = "ProjectRed-Transmission", version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.2]", dependencies = "required-after:ProjRed|Core;")
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
public class ProjectRedTransmission {

    /** Multipart items **/
    public static ItemPartWire itemPartWire;
    public static ItemPartFramedWire itemPartFramedWire;    
    
    /** Items **/
    public static ItemWireDebugger itemWireDebugger;

    @Instance("ProjRed|Transmission")
    public static ProjectRedTransmission instance;

    @SidedProxy(clientSide = "mrtjp.projectred.transmission.TransmissionClientProxy", serverSide = "mrtjp.projectred.transmission.TransmissionProxy")
    public static IProxy proxy;

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        MinecraftForge.EVENT_BUS.register(instance);
        MinecraftForge.EVENT_BUS.register(proxy);
        proxy.init();
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        proxy.postinit();
    }

}
