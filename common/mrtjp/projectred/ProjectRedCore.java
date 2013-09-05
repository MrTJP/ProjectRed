package mrtjp.projectred;

import mrtjp.projectred.api.APIImpl;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.core.BlockBasics;
import mrtjp.projectred.core.CommandDebug;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.CoreGuiHandler;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.ItemDrawPlate;
import mrtjp.projectred.core.ItemPart;
import mrtjp.projectred.core.ItemScrewdriver;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.common.network.NetworkMod;
import cpw.mods.fml.common.network.NetworkRegistry;
import cpw.mods.fml.common.registry.LanguageRegistry;

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
    
    public ProjectRedCore() {
        ProjectRedAPI.instance = new APIImpl();
    }
    
    /** Blocks **/
    public static BlockBasics blockMachines;

    /** Items **/
    public static ItemPart itemComponent;
    public static ItemDrawPlate itemDrawPlate;
    public static ItemScrewdriver itemScrewdriver;

    @Instance("ProjRed|Core")
    public static ProjectRedCore instance;
    
    @SidedProxy(clientSide = "mrtjp.projectred.core.CoreClientProxy", serverSide = "mrtjp.projectred.core.CoreProxy")
    public static IProxy proxy;    
    
    static {
        LanguageRegistry.instance().addStringLocalization("itemGroup.core", "en_US", "Project Red: Core");
        LanguageRegistry.instance().addStringLocalization("itemGroup.trans", "en_US", "Project Red: Transmission");
        LanguageRegistry.instance().addStringLocalization("itemGroup.int", "en_US", "Project Red: Integration");
        LanguageRegistry.instance().addStringLocalization("itemGroup.ill", "en_US", "Project Red: Illumination");
        LanguageRegistry.instance().addStringLocalization("itemGroup.expansion", "en_US", "Project Red: Expansion");
        LanguageRegistry.instance().addStringLocalization("itemGroup.exploration", "en_US", "Project Red: Exploration");
    }

    public static CreativeTabs tabCore = new CreativeTabs("core") {
        @Override
        public ItemStack getIconItemStack() {
            return new ItemStack(ProjectRedCore.itemScrewdriver);
        }
    };

    
    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        Configurator.initConfig(event);
        proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        MinecraftForge.EVENT_BUS.register(instance);
        NetworkRegistry.instance().registerGuiHandler(instance, new CoreGuiHandler());
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
