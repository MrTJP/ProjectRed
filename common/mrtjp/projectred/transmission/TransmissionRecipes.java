package mrtjp.projectred.transmission;

import mrtjp.projectred.ProjectRedTransmission;
import mrtjp.projectred.core.AlloySmelterRecipe;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.PRColors;
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
        initToolRecipes();
        initAlloySmelterRecipes();
    }

    private static void initToolRecipes() {
        /** Wire debugger **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedTransmission.itemWireDebugger), 
                "a a",
                "ber",
                "bgr",
                'a', EnumPart.REDINGOT.getItemStack(),
                'b', new ItemStack(Item.dyePowder, 1, PRColors.BLACK.dyeId()),
                'e', Item.emerald,
                'l', new ItemStack(Item.dyePowder, 1, PRColors.BLUE.dyeId()),
                'r', new ItemStack(Item.dyePowder, 1, PRColors.RED.dyeId()),
                'g', Item.glowstone
        );
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
        GameRegistry.addRecipe(new ShapedOreRecipe(EnumWire.BUNDLED_N.getItemStack(), 
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
                    PRColors.get(bundledColor).getOreDict(),
                    EnumWire.oreDictDefinitionBundled, 
                    PRColors.get(bundledColor).getOreDict(),
                    EnumWire.oreDictDefinitionBundled,
                    Item.silk,
                    Item.silk,
                    Item.silk
            ));
            bundledColor++;
        }
        
        /** Framed Wiring **/
        for (EnumWire w : EnumWire.VALID_WIRE) {
            if (w.hasFramedForm()) {
                // Regular to framed
                GameRegistry.addRecipe(w.getFramedItemStack(3), 
                        "sis",
                        "sis",
                        "sis",
                        'i', w.getItemStack(),
                        's', Item.stick
                );
                // framed to regular
                GameRegistry.addRecipe(w.getItemStack(3), 
                        "i",
                        "i",
                        "i",
                        'i', w.getFramedItemStack()
                );
            }
        }
        
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
        GameRegistry.addRecipe(EnumPart.BUNDLEDPLATE.getItemStack(), 
                "r",
                "p",
                'r', EnumWire.BUNDLED_N.getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
        );
    }
    
    private static void initAlloySmelterRecipes() {
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
