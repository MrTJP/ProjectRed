package mrtjp.projectred

import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.FMLInitializationEvent
import cpw.mods.fml.common.event.FMLPostInitializationEvent
import cpw.mods.fml.common.event.FMLPreInitializationEvent
import cpw.mods.fml.common.network.NetworkMod
import mrtjp.projectred.integration.{IntegrationProxy, EnumGate, ItemPartGate}
import net.minecraft.creativetab.CreativeTabs

@Mod(modid = "ProjRed|Integration", useMetadata = true, modLanguage = "scala")
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = classOf[PacketCustom.CustomTinyPacketHandler])
object ProjectRedIntegration
{
    /** Multipart items **/
    var itemPartGate:ItemPartGate = null

    var tabIntegration = new CreativeTabs("int")
    {
        override def getIconItemStack = EnumGate.Timer.getItemStack
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        IntegrationProxy.versionCheck()
        IntegrationProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        IntegrationProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        IntegrationProxy.postinit()
    }
}