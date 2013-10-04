package mrtjp.projectred.transmission;

import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.ShapedOreNBTRecipe;
import mrtjp.projectred.core.ShapelessOreNBTRecipe;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;
import codechicken.microblock.ItemMicroPart;
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
        for (EnumWire w : EnumWire.INSULATED_WIRE) // Recoloring
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(),
                    PRColors.get(w.meta-EnumWire.INSULATED_0.meta).getOreDict(),
                    EnumWire.oreDictDefinitionInsulated, 
                    Item.silk
            ));
        for (EnumWire w : EnumWire.INSULATED_WIRE) // Framed Recoloring
            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(w.getFramedItemStack(),
                    PRColors.get(w.meta-EnumWire.INSULATED_0.meta).getOreDict(),
                    EnumWire.oreDictDefinitionInsFramed, 
                    ItemMicroPart.create(769, Block.wood.getUnlocalizedName())
            ).setCheckNBT());
        
        
        /** Bundled Cables **/
        GameRegistry.addRecipe(new ShapedOreRecipe(EnumWire.BUNDLED_N.getItemStack(), 
                "SWS",
                "WWW",
                "SWS",
                'S', Item.silk,
                'W', EnumWire.oreDictDefinitionInsulated
        ));
        for (EnumWire w : EnumWire.BUNDLED_WIRE) { // recoloring
            if (w == EnumWire.BUNDLED_N)
                continue;
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(),
                    PRColors.get(w.meta-EnumWire.BUNDLED_0.meta).getOreDict(),
                    EnumWire.oreDictDefinitionBundled, 
                    Item.silk
            ));
        }
        
        /** Framed Wiring **/
        for (EnumWire w : EnumWire.VALID_WIRE)
            if (w.hasFramedForm()) {
                // Regular to framed
                GameRegistry.addRecipe(new ShapedOreNBTRecipe(w.getFramedItemStack(), 
                        "sss",
                        "sis",
                        "sss",
                        'i', w.getItemStack(),
                        's', ItemMicroPart.create(769, Block.wood.getUnlocalizedName())
                ).setCheckNBT());
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
}
