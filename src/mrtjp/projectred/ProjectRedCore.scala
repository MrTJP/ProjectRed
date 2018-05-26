package mrtjp.projectred

import mrtjp.projectred.core._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent, FMLServerStartingEvent}
import org.apache.logging.log4j.LogManager

@Mod(modid = "projectred-core", useMetadata = true, modLanguage = "scala", guiFactory = "mrtjp.projectred.core.GuiConfigFactory")
object ProjectRedCore
{
    val log = LogManager.getFormatterLogger("ProjectRed")

    /** Items **/
    var itemPart:ItemPart = _
    var itemDrawPlate:ItemDrawPlate = _
    var itemScrewdriver:ItemScrewdriver = _
    var itemMultimeter:ItemMultimeter = _

    val tabCore = new CreativeTabs("projectred.core")
    {
        override def getTabIconItem = new ItemStack(itemScrewdriver)
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        Configurator.loadConfig()
        CoreProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        CoreProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        CoreProxy.postinit()
    }

    @Mod.EventHandler
    def onServerStarting(event:FMLServerStartingEvent){}
}
