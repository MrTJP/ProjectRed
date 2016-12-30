package mrtjp.projectred

import mrtjp.projectred.illumination._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemBlock
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "projectred-illumination", useMetadata = true, modLanguage = "scala")
object ProjectRedIllumination
{
    /** Blocks **/
    var blockLamp:BlockLamp = _
    var itemBlockLamp:ItemBlock = _
    var blockAirousLight:BlockAirousLight = _

//    /** Multipart items **/
//    var itemPartIllumarButton:ItemPartButton = null
//    var itemPartIllumarFButton:ItemPartFButton = null

    var tabLighting = new CreativeTabs("projectred.illumination")
    {
        override def getTabIconItem = LightFactoryCage.getItem(true)
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