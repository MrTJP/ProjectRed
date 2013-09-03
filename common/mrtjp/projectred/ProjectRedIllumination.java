package mrtjp.projectred;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.illumination.EnumLantern;
import mrtjp.projectred.illumination.ItemPartLamp;
import mrtjp.projectred.illumination.ItemPartLantern;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.network.NetworkMod;

@Mod(modid = "ProjRed|Illumination", name = "ProjectRed-Illumination", version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.2]", dependencies = "required-after:ProjRed|Core;")
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
public class ProjectRedIllumination {

    /** Multipart items **/
    public static ItemPartLantern itemPartLantern;
    public static ItemPartLantern itemPartInvLantern;
    public static ItemPartLamp itemPartLamp;
    public static ItemPartLamp itemPartInvLamp;

    @Instance("ProjRed|Illumination")
    public static ProjectRedIllumination instance;

    @SidedProxy(clientSide = "mrtjp.projectred.illumination.IlluminationClientProxy", serverSide = "mrtjp.projectred.illumination.IlluminationProxy")
    public static IProxy proxy;

    public static CreativeTabs tabLighting = new CreativeTabs("ill") {
        @Override
        public ItemStack getIconItemStack() {
            return EnumLantern.RED.getInvertedItemStack();
        }
    };

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        if (Configurator.module_Illumination.getBoolean(true))
            proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        if (Configurator.module_Illumination.getBoolean(true)) {
            MinecraftForge.EVENT_BUS.register(instance);
            proxy.init();
        }
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        if (Configurator.module_Illumination.getBoolean(true))
            proxy.postinit();
    }

}
