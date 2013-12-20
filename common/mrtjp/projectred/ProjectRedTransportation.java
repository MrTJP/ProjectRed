package mrtjp.projectred;

import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.transportation.ItemPartPipe;
import mrtjp.projectred.transportation.ItemRouterUtility;
import mrtjp.projectred.transportation.ItemRoutingChip;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.Router;
import mrtjp.projectred.transportation.RouterServices;
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

@Mod(modid = "ProjRed|Transportation", useMetadata = true)
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = CustomTinyPacketHandler.class)
public class ProjectRedTransportation
{
    /** Items **/
    public static ItemRoutingChip itemRoutingChip;
    public static ItemRouterUtility itemRouterUtility;

    /** Multipart items **/
    public static ItemPartPipe itemPartPipe;

    @Instance("ProjRed|Transportation")
    public static ProjectRedTransportation instance;

    @SidedProxy(clientSide = "mrtjp.projectred.transportation.TransportationClientProxy", serverSide = "mrtjp.projectred.transportation.TransportationProxy")
    public static IProxy proxy;

    public static CreativeTabs tabTransportation = new CreativeTabs("transport") {
        @Override
        public ItemStack getIconItemStack()
        {
            return EnumRoutingChip.ITEMSTOCKKEEPER.getItemStack();
        }
    };

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

    @EventHandler
    public void serverStopping(FMLServerStoppingEvent event)
    {
        Router.reboot();
        RouterServices.reboot();
    }
}
