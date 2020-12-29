/*
package mrtjp.projectred

import mrtjp.projectred.integration.{GateDefinition, IntegrationProxy, ItemPartGate}
import net.minecraft.creativetab.CreativeTabs
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "projectred-integration", useMetadata = true, modLanguage = "scala")
object ProjectRedIntegration
{
    /** Multipart items **/
    var itemPartGate:ItemPartGate = _

    val tabIntegration = new CreativeTabs("projectred.integration")
    {
        override def getTabIconItem = GateDefinition.OR.makeStack
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
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
*/
