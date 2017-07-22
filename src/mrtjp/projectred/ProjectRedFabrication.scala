/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred

import codechicken.lib.model.bakery.sub.SubBlockBakery
import mrtjp.projectred.fabrication.{BlockICMachine, FabricationProxy, ItemICBlueprint, ItemICChip}
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "projectred-fabrication", useMetadata = true, modLanguage = "scala")
object ProjectRedFabrication
{
    /** Blocks **/
    var icBlock:BlockICMachine = _

    /** Items **/
    var itemICBlueprint:ItemICBlueprint = _
    var itemICChip:ItemICChip = _

    val icMachineBakery:SubBlockBakery = new SubBlockBakery

    var tabFabrication = new CreativeTabs("projectred.fabrication")
    {
        override def getTabIconItem = new ItemStack(itemICChip)
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        FabricationProxy.versionCheck()
        FabricationProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        FabricationProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        FabricationProxy.postinit()
    }
}