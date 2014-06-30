package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}
import mrtjp.projectred.integration.{EnumGate, IntegrationProxy, ItemPartGate}
import net.minecraft.creativetab.CreativeTabs

@Mod(modid = "ProjRed|Integration", useMetadata = true, modLanguage = "scala")
object ProjectRedIntegration
{
    /** Multipart items **/
    var itemPartGate:ItemPartGate = null

    var tabIntegration = new CreativeTabs("int")
    {
        override def getIconItemStack = EnumGate.Timer.makeStack
        override def getTabIconItem = getIconItemStack.getItem
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