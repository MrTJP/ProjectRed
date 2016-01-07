package mrtjp.projectred.compatibility.thermalexpansion

import cofh.api.modhelpers.ThermalExpansionHelper
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.{Configurator, PartDefs}
import net.minecraft.init.Items
import net.minecraft.item.ItemStack

object PluginThermalExpansion extends IPRPlugin
{
    override def getModIDs = Array("ThermalExpansion")

    override def isEnabled = Configurator.compat_TExpansion

    override def preInit() {}

    override def init()
    {
        //     cant be used if tesseracts dont auto-eject into pipes.
//        if (ProjectRedAPI.transportationAPI != null)
//            ProjectRedAPI.transportationAPI.registerSpecialLinkState(new LinkStateTesseract());

//        SmelterManager.addAlloyRecipe(4000, new ItemStack(Items.iron_ingot),
//            new ItemStack(Items.redstone, 4), PartDefs.REDINGOT.makeStack)

        ThermalExpansionHelper.addSmelterRecipe(4000, new ItemStack(Items.iron_ingot), new ItemStack(Items.redstone, 4),
            PartDefs.REDINGOT.makeStack)

    }

    override def postInit() {}

    override def desc() = "Thermal Expansion: machine recipes"
}