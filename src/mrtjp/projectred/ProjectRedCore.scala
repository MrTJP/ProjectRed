package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.FMLInitializationEvent
import cpw.mods.fml.common.event.FMLPostInitializationEvent
import cpw.mods.fml.common.event.FMLPreInitializationEvent
import cpw.mods.fml.common.event.FMLServerStartingEvent
import mrtjp.projectred.core._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack
import net.minecraftforge.common.MinecraftForge

@Mod(modid = "ProjRed|Core", useMetadata = true, modLanguage = "scala")
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
        CoreProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        CoreProxy.postinit()
        MinecraftForge.EVENT_BUS.register(new PRVersioningThread)
    }

    @Mod.EventHandler
    def onServerStarting(event:FMLServerStartingEvent)
    {
        event.registerServerCommand(new CommandDebug)
    }
}
