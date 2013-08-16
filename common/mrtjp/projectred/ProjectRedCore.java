package mrtjp.projectred;

import mrtjp.projectred.core.CommandDebug;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.ItemDrawPlate;
import mrtjp.projectred.core.ItemPart;
import mrtjp.projectred.expansion.BlockMachines;
import mrtjp.projectred.expansion.ItemVAWT;
import mrtjp.projectred.exploration.BlockOre;
import mrtjp.projectred.exploration.BlockSpecialStone;
import mrtjp.projectred.exploration.ItemBackpack;
import mrtjp.projectred.exploration.ItemGemAxe;
import mrtjp.projectred.exploration.ItemGemHoe;
import mrtjp.projectred.exploration.ItemGemPickaxe;
import mrtjp.projectred.exploration.ItemGemSaw;
import mrtjp.projectred.exploration.ItemGemShovel;
import mrtjp.projectred.exploration.ItemGemSickle;
import mrtjp.projectred.exploration.ItemGemSword;
import mrtjp.projectred.exploration.ItemWoolGin;
import mrtjp.projectred.illumination.ItemPartLamp;
import mrtjp.projectred.illumination.ItemPartLantern;
import mrtjp.projectred.integration.ItemPartGate;
import mrtjp.projectred.integration.ItemScrewdriver;
import mrtjp.projectred.transmission.ItemPartJacketedWire;
import mrtjp.projectred.transmission.ItemPartWire;
import mrtjp.projectred.transmission.ItemWireDebugger;
import net.minecraft.item.EnumToolMaterial;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.common.network.NetworkMod;

/**
 * "Project: Red" serves to provide a complete alternative for Eloraam's
 * RedPower 2 utilizing ForgeMultipart API created by Chickenbones.
 * 
 * @author MrTJP
 * 
 */
@Mod(modid = "ProjRed|Core", name = "ProjectRed-Core", version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.2]",
dependencies = 
        "required-after:Forge@FORGE_VERSION@;" +
        "required-after:ForgeMultipart;" +
        "required-after:CodeChickenCore;" +
        "after:CCTurtle;" +
        "after:ComputerCraft;"
)
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
public class ProjectRedCore {
    
    /** Items **/
    public static ItemPart itemComponent;
    public static ItemDrawPlate itemDrawPlate;

    @Instance("ProjRed|Core")
    public static ProjectRedCore instance;
    
    @SidedProxy(clientSide = "mrtjp.projectred.core.CoreClientProxy", serverSide = "mrtjp.projectred.core.CoreProxy")
    public static IProxy proxy;    
    
    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        Configurator.initConfig(event);
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

    @Mod.EventHandler
    public void onServerStarting(FMLServerStartingEvent event) {
        event.registerServerCommand(new CommandDebug());
    }

}
