package mrtjp.projectred.compatibility.thermalexpansion;

import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import thermalexpansion.util.crafting.SmelterManager;

public class ProxyThermalExpansion
{
    public void init()
    {

     // cant be used if tesseracts dont auto-eject into pipes.
//        if (ProjectRedAPI.transportationAPI != null)
//            ProjectRedAPI.transportationAPI.registerSpecialLinkState(new LinkStateTesseract());
        

        SmelterManager.addAlloyRecipe(4000, new ItemStack(Item.ingotIron), new ItemStack(Item.redstone, 4), EnumPart.REDINGOT.getItemStack());
    }
}
