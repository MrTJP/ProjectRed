package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.GameRegistry;

public class IlluminationRecipes {

    public static void initIlluminationRecipes() {
        initLightingRecipes();
    }

    private static void initLightingRecipes() {
        /** Lamps **/
        for (EnumLamp l : EnumLamp.VALID_TYPES) {
            GameRegistry.addRecipe(l.getItemStack(),  // Regular
                    "gIg",
                    "gIg",
                    "gtg",
                    'g', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
                    't', Item.redstone
            );
            GameRegistry.addRecipe(l.getInvertedItemStack(), // Inverted
                    "gIg",
                    "gIg",
                    "gtg",
                    'g', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
                    't', Block.torchRedstoneActive
            );
        }

        /** Lanterns **/
        for (EnumLantern l : EnumLantern.VALID_TYPES) {
            GameRegistry.addRecipe(l.getItemStack(), 
                    "PNP",
                    "GIG",
                    "PRP",
                    'P', EnumPart.PLATE.getItemStack(),
                    'N', Item.goldNugget,
                    'G', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
                    'R', Item.redstone    
            );
            GameRegistry.addRecipe(l.getInvertedItemStack(), 
                    "PNP",
                    "GIG",
                    "PRP",
                    'P', EnumPart.PLATE.getItemStack(),
                    'N', Item.goldNugget,
                    'G', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[l.meta].getItemStack(),
                    'R', Block.torchRedstoneActive
            );
        }
        
        /** Buttons **/
        for (int i = 0; i < 16; i++)
            GameRegistry.addShapelessRecipe(
                    new ItemStack(ProjectRedIllumination.itemPartIlluminatedButton, 1, i), 
                    Block.stoneButton, 
                    EnumPart.ILLUMAR_PARTS[i].getItemStack(), 
                    EnumPart.ILLUMAR_PARTS[i].getItemStack()
            );
    }
}

