package mrtjp.projectred

import mrtjp.projectred.api.ProjectRedAPI
import mrtjp.projectred.transportation._
import net.minecraft.creativetab.CreativeTabs
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent, FMLServerStoppingEvent}

@Mod(modid = "projectred-transportation", useMetadata = true, modLanguage = "scala")
object ProjectRedTransportation
{
    ProjectRedAPI.transportationAPI = new APIImpl_Transportation

    /** Items **/
    var itemRoutingChip:ItemRoutingChip = _
    var itemRouterUtility:ItemRouterUtility = _

    /** Multipart items **/
    var itemPartPipe:ItemPartPipe = _

    val tabTransportation = new CreativeTabs("projectred.transportation")
    {
        override def getTabIconItem = RoutingChipDefs.ITEMSTOCKKEEPER.makeStack
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
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
