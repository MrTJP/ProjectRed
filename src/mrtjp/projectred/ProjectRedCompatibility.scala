package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}
import mrtjp.projectred.compatibility.CompatibilityProxy

@Mod(modid = "ProjRed|Compatibility", useMetadata = true, dependencies = "after:ProjRed|Core", modLanguage = "scala")
object ProjectRedCompatibility
{
    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        CompatibilityProxy.versionCheck()
        CompatibilityProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        CompatibilityProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        CompatibilityProxy.postinit()
    }
}