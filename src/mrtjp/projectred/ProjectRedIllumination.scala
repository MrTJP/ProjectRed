package mrtjp.projectred

import mrtjp.projectred.illumination._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemBlock
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "ProjRed|Illumination", useMetadata = true, modLanguage = "scala")
object ProjectRedIllumination
{
    /** Blocks **/
    var blockLamp:BlockLamp = null
    var itemBlockLamp:ItemBlock = null
    var blockAirousLight:BlockAirousLight = null

//    /** Multipart items **/
//    var itemPartIllumarButton:ItemPartButton = null
//    var itemPartIllumarFButton:ItemPartFButton = null

    var tabLighting = new CreativeTabs("PR|illumination")
    {
        //override def getTabIconItem = //LightObjCage.getItem(true)
        override def getTabIconItem = itemBlockLamp
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