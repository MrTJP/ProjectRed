package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}
import mrtjp.projectred.expansion._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Expansion", useMetadata = true, modLanguage = "scala")
object ProjectRedExpansion
{
    /** Blocks **/
    var machine1:BlockMachine = null //machines
    var machine2:BlockMachine = null //devices

    /** Items **/
    var itemEmptybattery:ItemBatteryEmpty = null
    var itemBattery:ItemBattery = null
    var itemJetpack:ItemElectronicJetpack = null
    var itemScrewdriver:ItemElectronicScrewdriver = null

    /** Parts **/
    var itemSolar:ItemSolarPanel = null

    val tabExpansion = new CreativeTabs("expansion")
    {
        override def getIconItemStack = new ItemStack(machine2, 1, 0)
        override def getTabIconItem = getIconItemStack.getItem
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        ExpansionProxy.versionCheck()
        ExpansionProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        ExpansionProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        ExpansionProxy.postinit()
    }
}
