package mrtjp.projectred

import mrtjp.projectred.illumination._
import net.minecraftforge.fml.DistExecutor

object ProjectRedIllumination {
    final var MOD_ID = "projectred-illumination"
    final var proxy:IlluminationProxy = DistExecutor.safeRunForDist(
        () => () => new IlluminationProxyClient().asInstanceOf[IlluminationProxy],
        () => () => new IlluminationProxy())
}

class ProjectRedIllumination {
    ProjectRedIllumination.proxy.construct()
}


//
//@Mod(modid = "projectred-illumination", useMetadata = true, modLanguage = "scala")
//object ProjectRedIllumination
//{
//    /** Blocks **/
//    var blockLamp:BlockLamp = _
//    var itemBlockLamp:ItemBlock = _
//    var blockAirousLight:BlockAirousLight = _
//
////    /** Multipart items **/
//    var itemPartIllumarButton:ItemPartButton = _
//    var itemPartIllumarFButton:ItemPartFButton = _
//
//    val tabLighting = new CreativeTabs("projectred.illumination")
//    {
//        override def getTabIconItem = new ItemStack(LightFactoryCage.getItem(true))
//    }
//
//    @Mod.EventHandler
//    def preInit(event:FMLPreInitializationEvent)
//    {
//        IlluminationProxy.preinit()
//    }
//
//    @Mod.EventHandler
//    def init(event:FMLInitializationEvent)
//    {
//        IlluminationProxy.init()
//    }
//
//    @Mod.EventHandler
//    def postInit(event:FMLPostInitializationEvent)
//    {
//        IlluminationProxy.postinit()
//    }
//}

