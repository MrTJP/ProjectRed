package mrtjp.projectred

import mrtjp.projectred.api.ProjectRedAPI
import mrtjp.projectred.core.IProxy
import mrtjp.projectred.transmission.{TransmissionProxy, APIImpl_Transmission, ItemPartFramedWire, ItemPartWire}
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack
import net.minecraftforge.common.MinecraftForge
import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.Mod.Instance
import cpw.mods.fml.common.SidedProxy
import cpw.mods.fml.common.event.FMLInitializationEvent
import cpw.mods.fml.common.event.FMLPostInitializationEvent
import cpw.mods.fml.common.event.FMLPreInitializationEvent
import cpw.mods.fml.common.network.NetworkMod

@Mod(modid = "ProjRed|Transmission", useMetadata = true, modLanguage = "scala")
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
object ProjectRedTransmission
{
    ProjectRedAPI.transmissionAPI = new APIImpl_Transmission

    /** Multipart items **/
    var itemPartWire:ItemPartWire = null
    var itemPartFramedWire:ItemPartFramedWire = null

    var tabTransmission = new CreativeTabs("trans")
    {
        override def getIconItemStack = new ItemStack(ProjectRedTransmission.itemPartWire)
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
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
}