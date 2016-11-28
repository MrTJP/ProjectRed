package mrtjp.projectred

import mrtjp.projectred.api.ProjectRedAPI
import mrtjp.projectred.transportation._
import net.minecraft.creativetab.CreativeTabs
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent, FMLServerStoppingEvent}

@Mod(modid = "ProjRed|Transportation", useMetadata = true, modLanguage = "scala")
object ProjectRedTransportation
{
    ProjectRedAPI.transportationAPI = new APIImpl_Transportation

    /** Items **/
    var itemRoutingChip:ItemRoutingChip = null
    var itemRouterUtility:ItemRouterUtility = null

    /** Multipart items **/
    var itemPartPipe:ItemPartPipe = null

    var tabTransportation = new CreativeTabs("transport")
    {
        override def getIconItemStack = RoutingChipDefs.ITEMSTOCKKEEPER.makeStack
        override def getTabIconItem = getIconItemStack.getItem
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
        RouterServices.reboot()
    }
}