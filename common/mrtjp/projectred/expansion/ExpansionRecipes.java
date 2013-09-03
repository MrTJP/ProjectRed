package mrtjp.projectred.expansion;

import mrtjp.projectred.ProjectRedExpansion;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.transmission.EnumWire;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.GameRegistry;

public class ExpansionRecipes {
    
    public static void initRecipes() {
        initMiscRecipes();
    }
    
    private static void initMiscRecipes() {        
        /** VAWT **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedExpansion.itemVAWT, 1), 
                "sss",
                "ttt",
                "sss",
                's', EnumPart.SAIL.getItemStack(),
                't', Item.stick
        );
        GameRegistry.addRecipe(new RecipeVAWTRecoloring());
    }
}

