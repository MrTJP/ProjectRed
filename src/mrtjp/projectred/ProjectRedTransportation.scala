package mrtjp.projectred

import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.FMLInitializationEvent
import cpw.mods.fml.common.event.FMLPostInitializationEvent
import cpw.mods.fml.common.event.FMLPreInitializationEvent
import cpw.mods.fml.common.event.FMLServerStoppingEvent
import cpw.mods.fml.common.network.NetworkMod
import mrtjp.projectred.api.ProjectRedAPI
import mrtjp.projectred.transportation._
import net.minecraft.creativetab.CreativeTabs
import mrtjp.projectred.expansion.ItemCPU

@Mod(modid = "ProjRed|Transportation", useMetadata = true, modLanguage = "scala")
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = classOf[PacketCustom.CustomTinyPacketHandler])
object ProjectRedTransportation
{
    ProjectRedAPI.transportationAPI = new APIImpl_Transportation

    /** Items **/
    var itemRoutingChip:ItemRoutingChip = null
    var itemRouterUtility:ItemRouterUtility = null
    var itemRouterCPU:ItemCPU = null

    /** Multipart items **/
    var itemPartPipe:ItemPartPipe = null

    var tabTransportation = new CreativeTabs("transport")
    {
        override def getIconItemStack = EnumRoutingChip.ITEMSTOCKKEEPER.getItemStack
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        TransportationProxy.versionCheck()
        TransportationProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        TransportationProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        TransportationProxy.postinit()
    }

    @Mod.EventHandler
    def serverStopping(event:FMLServerStoppingEvent)
    {
        Router.reboot()
        RouterServices.reboot()
    }
}