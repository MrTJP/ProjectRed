package mrtjp.projectred.compatibility.thermalexpansion

import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.ItemPart.EnumPart
import net.minecraft.item.{Item, ItemStack}
import thermalexpansion.util.crafting.SmelterManager

class PluginTermalExpansion extends IPRPlugin
{
    override def getModID = "ThermalExpansion"

    override def preInit() {}

    override def init()
    {
        //     cant be used if tesseracts dont auto-eject into pipes.
//        if (ProjectRedAPI.transportationAPI != null)
//            ProjectRedAPI.transportationAPI.registerSpecialLinkState(new LinkStateTesseract());

        SmelterManager.addAlloyRecipe(4000, new ItemStack(Item.ingotIron),
            new ItemStack(Item.redstone, 4), EnumPart.REDINGOT.getItemStack)
    }

    override def postInit() {}
}