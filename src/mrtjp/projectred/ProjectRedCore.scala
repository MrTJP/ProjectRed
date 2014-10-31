package mrtjp.projectred

import cpw.mods.fml.common.{FMLCommonHandler, Mod}
import cpw.mods.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent, FMLServerStartingEvent}
import mrtjp.projectred.core._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Core", useMetadata = true, modLanguage = "scala", guiFactory = "mrtjp.projectred.core.GuiConfigFactory")
object ProjectRedCore
{
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
        Configurator.initConfig(event)
        CoreProxy.versionCheck()
        CoreProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        FMLCommonHandler.instance().bus().register(new Configurator)
        CoreProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        CoreProxy.postinit()
    }

    @Mod.EventHandler
    def onServerStarting(event:FMLServerStartingEvent)
    {
    }
}
