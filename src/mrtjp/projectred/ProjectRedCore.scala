package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent, FMLServerStartingEvent}
import mrtjp.projectred.core._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack
import org.apache.logging.log4j.LogManager

@Mod(modid = "ProjRed|Core", useMetadata = true, modLanguage = "scala", guiFactory = "mrtjp.projectred.core.GuiConfigFactory")
object ProjectRedCore
{
    val log = LogManager.getFormatterLogger("ProjRed|Core")

    /** Items **/
    var itemPart:ItemPart = null
    var itemDrawPlate:ItemDrawPlate = null
    var itemScrewdriver:ItemScrewdriver = null
    var itemWireDebugger:ItemWireDebugger = null
    var itemDataCard:ItemDataCard = null

    var tabCore = new CreativeTabs("core")
    {
        override def getIconItemStack = new ItemStack(ProjectRedCore.itemScrewdriver)
        override def getTabIconItem = getIconItemStack.getItem
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        Configurator.loadConfig()
        CoreProxy.versionCheck()
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