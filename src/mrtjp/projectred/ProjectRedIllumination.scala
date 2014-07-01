package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}
import mrtjp.projectred.core.libmc.PRColors
import mrtjp.projectred.illumination._
import net.minecraft.creativetab.CreativeTabs

@Mod(modid = "ProjRed|Illumination", useMetadata = true, modLanguage = "scala")
object ProjectRedIllumination
{
    /** Blocks **/
    var blockLamp:BlockLamp = null
    var blockAirousLight:BlockAirousLight = null

    /** Multipart items **/
    var itemPartIllumarButton:ItemPartButton = null
    var itemPartIllumarFButton:ItemPartFButton = null

    var tabLighting = new CreativeTabs("ill")
    {
        override def getIconItemStack = LightObjLantern.makeInvStack(PRColors.RED.ordinal())
        override def getTabIconItem = getIconItemStack.getItem
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        IlluminationProxy.versionCheck()
        IlluminationProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        IlluminationProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        IlluminationProxy.postinit()
    }
}