package mrtjp.projectred;

import mrtjp.projectred.core.BlockBasics;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import mrtjp.projectred.expansion.ExpansionGuiHandler;
import mrtjp.projectred.expansion.ItemPartPressurizedTube;
import mrtjp.projectred.expansion.ItemVAWT;
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
import cpw.mods.fml.common.network.NetworkRegistry;

@Mod(modid = "ProjRed|Expansion", name = "ProjectRed-Expansion", version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.2]", dependencies = "required-after:ProjRed|Core;")
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
public class ProjectRedExpansion {

    /** Items **/
    public static ItemVAWT itemVAWT;

    /** Multipart items **/
    public static ItemPartPressurizedTube itemPartTube;

    @Instance("ProjRed|Expansion")
    public static ProjectRedExpansion instance;

    @SidedProxy(clientSide = "mrtjp.projectred.expansion.ExpansionClientProxy", serverSide = "mrtjp.projectred.expansion.ExpansionProxy")
    public static IProxy proxy;

    public static CreativeTabs tabExpansion = new CreativeTabs("expansion") {
        @Override
        public ItemStack getIconItemStack() {
            return EnumBasics.ALLOYSMELTER.getItemStack();
        }
    };

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        MinecraftForge.EVENT_BUS.register(instance);
        NetworkRegistry.instance().registerGuiHandler(instance, new ExpansionGuiHandler());
        proxy.init();
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        proxy.postinit();
    }

}
