package mrtjp.projectred

import mrtjp.projectred.api.ProjectRedAPI
import mrtjp.projectred.transmission._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLServerAboutToStartEvent, FMLPostInitializationEvent, FMLInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "projectred-transmission", useMetadata = true, modLanguage = "scala")
object ProjectRedTransmission
{
    ProjectRedAPI.transmissionAPI = new APIImpl_Transmission

    /** Multipart items **/
    var itemPartWire:ItemPartWire = _
    var itemPartFramedWire:ItemPartFramedWire = _

    var tabTransmission = new CreativeTabs("projectred.transmission")
    {
        override def getIconItemStack = new ItemStack(ProjectRedTransmission.itemPartWire)
        override def getTabIconItem = getIconItemStack.getItem
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        TransmissionProxy.versionCheck()
        TransmissionProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        TransmissionProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        TransmissionProxy.postinit()
    }

    @Mod.EventHandler
    def serverStopping(event:FMLServerAboutToStartEvent)
    {
        WirePropagator.reset()
    }
}