package mrtjp.projectred.transmission;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;
import cpw.mods.fml.common.registry.GameRegistry;

public class TransmissionRecipes {
    
    public static void initTransmissionRecipes() {
        initWireRecipes();
        initPartRecipes();
    }

    private static void initWireRecipes() {    
        /** Red Alloy Wires **/
        GameRegistry.addRecipe(EnumWire.RED_ALLOY.getItemStack(12), 
                " r ",
                " r ",
                " r ",
                'r', EnumPart.REDINGOT.getItemStack()
        );
        
        /** Insulated Wires **/
        for (int i = 0; i < EnumWire.INSULATED_WIRE.length; i++) {
            EnumWire w = EnumWire.INSULATED_WIRE[i];
            GameRegistry.addRecipe(w.getItemStack(12), 
                    "WrW",
                    "WrW",
                    "WrW",
                    'W', new ItemStack(Block.cloth, 1, PRColors.get(i).woolId()),
                    'r', EnumPart.REDINGOT.getItemStack()
            );
        }
        
        /** Bundled Cables **/
        /*GameRegistry.addRecipe(new ShapedOreRecipe(EnumWire.BUNDLED_N.getItemStack(), 
                "SWS",
                "WWW",
                "SWS",
                'S', Item.silk,
                'W', EnumWire.oreDictDefinitionInsulated
        ));
        int bundledColor = 0;
        for (EnumWire w : EnumWire.BUNDLED_WIRE) {
            if (w == EnumWire.BUNDLED_N) {
                continue;
            }
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(3),
                    PRColors.get(bundledColor).getOreDict(),
                    EnumWire.oreDictDefinitionBundled, 
                    EnumWire.oreDictDefinitionBundled, 
                    EnumWire.oreDictDefinitionBundled, 
                    PRColors.get(bundledColor).getOreDict()
            ));
            bundledColor++;
        }*/
        
        /** Jacketed Wiring **/
        /*for (EnumWire w : EnumWire.VALID_WIRE) {
            if (w.hasJacketedForm()) {
                // Regular to jacketed
                GameRegistry.addRecipe(w.getJacketedItemStack(3), 
                        "sis",
                        "sis",
                        "sis",
                        'i', w.getItemStack(),
                        's', Item.stick
                );
                // Jacketed to regular
                GameRegistry.addRecipe(w.getItemStack(3), 
                        "i",
                        "i",
                        "i",
                        'i', w.getJacketedItemStack()
                );
            }
        }*/
        
    }
    private static void initPartRecipes() {
        /** Wired Plate **/
        GameRegistry.addRecipe(EnumPart.WIREDPLATE.getItemStack(), 
                "r",
                "p",
                'r', EnumWire.RED_ALLOY.getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
        );

        /** Bundled Plate **/
        /*GameRegistry.addRecipe(EnumPart.BUNDLEDPLATE.getItemStack(), 
                "r",
                "p",
                'r', EnumWire.BUNDLED_N.getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
        );*/
    }
}
