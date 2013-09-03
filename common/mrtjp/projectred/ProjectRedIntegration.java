package mrtjp.projectred;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.integration.EnumGate;
import mrtjp.projectred.integration.ItemPartGate;
import mrtjp.projectred.integration.ItemScrewdriver;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom.CustomTinyPacketHandler;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.network.NetworkMod;

@Mod(modid = "ProjRed|Integration", name = "ProjectRed-Integration", version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.2]", dependencies = "required-after:ProjRed|Core;")
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = CustomTinyPacketHandler.class)
public class ProjectRedIntegration {

    /** Multipart items **/
    public static ItemPartGate itemPartGate2;

    /** Items **/
    public static ItemScrewdriver itemScrewdriver;

    @Instance("ProjRed|Integration")
    public static ProjectRedIntegration instance;

    @SidedProxy(clientSide = "mrtjp.projectred.integration.IntegrationClientProxy", serverSide = "mrtjp.projectred.integration.IntegrationProxy")
    public static IProxy proxy;

    public static CreativeTabs tabIntegration = new CreativeTabs("int") {
        @Override
        public ItemStack getIconItemStack() {
            return EnumGate.Timer.getItemStack();
        }
    };

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        if (Configurator.module_Integration.getBoolean(true))
            proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        if (Configurator.module_Integration.getBoolean(true)) {
            MinecraftForge.EVENT_BUS.register(instance);
            proxy.init();
        }
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        if (Configurator.module_Integration.getBoolean(true))
            proxy.postinit();
    }
}
