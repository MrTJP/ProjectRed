/*
package mrtjp.projectred

import codechicken.lib.model.bakery.sub.SubBlockBakery
import mrtjp.projectred.expansion._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.{Item, ItemStack}
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "projectred-expansion", useMetadata = true, modLanguage = "scala")
object ProjectRedExpansion
{
    /** Blocks **/
    var machine1:BlockMachine = _ //machines
    var machine2:BlockMachine = _ //devices

    /** Items **/
    var itemEmptybattery:ItemEmptyBattery = _
    var itemBattery:ItemBattery = _
    var itemJetpack:ItemJetpack = _
    var itemScrewdriver:ItemElectricScrewdriver = _
    var itemInfusedEnderPearl:ItemInfusedEnderPearl = _
    var itemPlan:ItemPlan = _

    /** Enchantments **/
    var enchantmentElectricEfficiency:EnchantmentElectricEfficiency = _

    /** Parts **/
    var itemSolar:ItemSolarPanel = _

    val tabExpansion = new CreativeTabs("projectred.expansion")
    {
        override def getTabIconItem = new ItemStack(machine2)
    }

    val machine1Bakery:SubBlockBakery = new SubBlockBakery
    val machine2Bakery:SubBlockBakery = new SubBlockBakery

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
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
*/
