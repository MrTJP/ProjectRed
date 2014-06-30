package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event._
import mrtjp.projectred.api.ProjectRedAPI
import mrtjp.projectred.transmission._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Transmission", useMetadata = true, modLanguage = "scala")
object ProjectRedTransmission
{
    ProjectRedAPI.transmissionAPI = new APIImpl_Transmission

    /** Multipart items **/
    var itemPartWire:ItemPartWire = null
    var itemPartFramedWire:ItemPartFramedWire = null

    var tabTransmission = new CreativeTabs("trans")
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