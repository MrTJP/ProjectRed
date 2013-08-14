package mrtjp.projectred.expansion;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import mrtjp.projectred.transmission.EnumWire;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.GameRegistry;

public class ExpansionRecipes {
    
    public static void initRecipes() {
        initMachineRecipes();
        initToolRecipes();
        initOtherAlloySmelterRecipes();
    }
    
    private static void initMachineRecipes() {
        
        /** Alloy Smelter **/
        GameRegistry.addRecipe(EnumMachine.ALLOYSMELTER.getItemStack(), 
                "CBC",
                "BBB",
                "CBC",
                'C', Block.blockClay,
                'B', Block.brick
        );
        
    }    
    private static void initToolRecipes() {        
        /** VAWT **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedCore.itemVAWT, 1), 
                "sss",
                "ttt",
                "sss",
                's', EnumPart.SAIL.getItemStack(),
                't', Item.stick
        );
        GameRegistry.addRecipe(new RecipeVAWTRecoloring());
    }
    
    private static void initOtherAlloySmelterRecipes() {
        /** Red Alloy Ingot reset recipes **/
        AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
                EnumWire.RED_ALLOY.getItemStack(4),
        }, EnumPart.REDINGOT.getItemStack(), 50));
        AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
                EnumWire.BUNDLED_N.getItemStack(8),
        }, EnumPart.REDINGOT.getItemStack(5), 90));
        for (EnumWire w : EnumWire.INSULATED_WIRE) {
            AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
                    w.getItemStack(4)
            }, EnumPart.REDINGOT.getItemStack(), 80));
        }
    }


}

