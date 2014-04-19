package mrtjp.projectred

import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.FMLInitializationEvent
import cpw.mods.fml.common.event.FMLPostInitializationEvent
import cpw.mods.fml.common.event.FMLPreInitializationEvent
import cpw.mods.fml.common.event.FMLServerStartingEvent
import cpw.mods.fml.common.network.NetworkMod
import cpw.mods.fml.common.registry.TickRegistry
import cpw.mods.fml.relauncher.Side
import mrtjp.projectred.core._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Core", useMetadata = true, modLanguage = "scala")
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = classOf[PacketCustom.CustomTinyPacketHandler])
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
        TickRegistry.registerTickHandler(new PRVersioningThread, Side.CLIENT)
    }

    @Mod.EventHandler
    def onServerStarting(event:FMLServerStartingEvent)
    {
        event.registerServerCommand(new CommandDebug)
    }
}
