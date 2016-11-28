package mrtjp.projectred

import mrtjp.projectred.integration.{GateDefinition, IntegrationProxy, ItemPartGate}
import net.minecraft.creativetab.CreativeTabs
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "ProjRed|Integration", useMetadata = true, modLanguage = "scala")
object ProjectRedIntegration
{
    /** Multipart items **/
    var itemPartGate:ItemPartGate = null

    var tabIntegration = new CreativeTabs("int")
    {
        override def getIconItemStack = GateDefinition.OR.makeStack
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