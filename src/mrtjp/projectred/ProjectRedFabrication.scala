/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}
import mrtjp.projectred.fabrication.{ItemICBlueprint, BlockICMachine, FabricationProxy}
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.init.Items
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Fabrication", useMetadata = true, modLanguage = "scala")
object ProjectRedFabrication
{
    /** Blocks **/
    var icBlock:BlockICMachine = null

    /** Items **/
    var itemICBlueprint:ItemICBlueprint = null

    var tabFabrication = new CreativeTabs("fab")
    {
        override def getIconItemStack = new ItemStack(Items.nether_star)
        override def getTabIconItem = getIconItemStack.getItem
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