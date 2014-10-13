package mrtjp.projectred.compatibility.thermalexpansion

import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.PartDefs
import net.minecraft.init.Items
import net.minecraft.item.ItemStack
import thermalexpansion.util.crafting.SmelterManager

object PluginThermalExpansion extends IPRPlugin
{

    override def getModIDs = Array("ThermalExpansion")

    override def preInit() {}

    override def init()
    {
        //     cant be used if tesseracts dont auto-eject into pipes.
//        if (ProjectRedAPI.transportationAPI != null)
//            ProjectRedAPI.transportationAPI.registerSpecialLinkState(new LinkStateTesseract());

        SmelterManager.addAlloyRecipe(4000, new ItemStack(Items.iron_ingot),
            new ItemStack(Items.redstone, 4), PartDefs.REDINGOT.makeStack)
    }

    override def postInit() {}

    override def desc() = "Thermal Expansion smelter recipe"
}
