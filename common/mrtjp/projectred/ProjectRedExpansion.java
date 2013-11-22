package mrtjp.projectred;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.expansion.ItemPartPipe;
import mrtjp.projectred.expansion.ItemRoutingChip;
import mrtjp.projectred.expansion.Router;
import mrtjp.projectred.expansion.RouterServices;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom.CustomTinyPacketHandler;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.EventHandler;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStoppingEvent;
import cpw.mods.fml.common.network.NetworkMod;

@Mod(modid = "ProjRed|Expansion", name = "ProjectRed-Expansion", version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.4]", dependencies = "required-after:ProjRed|Core;")
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = CustomTinyPacketHandler.class)
public class ProjectRedExpansion {

    /** Items **/
    public static ItemRoutingChip itemRoutingChip;

    /** Multipart items **/
    public static ItemPartPipe itemPartPipe;

    @Instance("ProjRed|Expansion")
    public static ProjectRedExpansion instance;

    @SidedProxy(clientSide = "mrtjp.projectred.expansion.ExpansionClientProxy", serverSide = "mrtjp.projectred.expansion.ExpansionProxy")
    public static IProxy proxy;

    public static CreativeTabs tabExpansion = new CreativeTabs("expansion") {
        @Override
        public ItemStack getIconItemStack() {
            return EnumPart.COPPERCOIL.getItemStack();
        }
    };

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        MinecraftForge.EVENT_BUS.register(instance);
        proxy.init();
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        proxy.postinit();
    }

    @EventHandler
    public void serverStopping(FMLServerStoppingEvent event) {
        Router.reboot();
        RouterServices.reboot();
    }
}
