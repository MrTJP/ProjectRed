package mrtjp.projectred.transmission;

import codechicken.microblock.ItemMicroPart;
import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.ShapedOreNBTRecipe;
import mrtjp.projectred.core.ShapelessOreNBTRecipe;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;

public class TransmissionRecipes
{
    public static void initTransmissionRecipes()
    {
        initWireRecipes();
        initPartRecipes();
    }

    private static void initWireRecipes()
    {
        /** Red Alloy Wires **/
        GameRegistry.addRecipe(WireDef.RED_ALLOY().getItemStack(12),
                " r ",
                " r ",
                " r ",
                'r', EnumPart.REDINGOT.getItemStack()
                );

        /** Insulated Wires **/
        for (int i = 0; i < WireDef.INSULATED_WIRE().length; i++)
        {
            WireDef w = WireDef.INSULATED_WIRE()[i];
            GameRegistry.addRecipe(w.getItemStack(12),
                    "WrW",
                    "WrW",
                    "WrW",
                    'W', new ItemStack(Block.cloth, 1, PRColors.get(i).woolId()),
                    'r', EnumPart.REDINGOT.getItemStack()
                    );
        }
        for (WireDef w : WireDef.INSULATED_WIRE()) // Recoloring
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(),
                    PRColors.get(w.meta()-WireDef.INSULATED_0().meta()).getOreDict(),
                    WireDef.oreDictDefinitionInsulated(),
                    Item.silk
                    ));
        for (WireDef w : WireDef.INSULATED_WIRE()) // Framed Recoloring
            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(w.getFramedItemStack(),
                    PRColors.get(w.meta()-WireDef.INSULATED_0().meta()).getOreDict(),
                    WireDef.oreDictDefinitionInsFramed(),
                    ItemMicroPart.create(769, Block.wood.getUnlocalizedName())
                    ).setCheckNBT());


        /** Bundled Cables **/
        GameRegistry.addRecipe(new ShapedOreRecipe(WireDef.BUNDLED_N().getItemStack(),
                "SWS",
                "WWW",
                "SWS",
                'S', Item.silk,
                'W', WireDef.oreDictDefinitionInsulated()
                ));
        for (WireDef w : WireDef.BUNDLED_WIRE()) // recoloring
        {
            if (w == WireDef.BUNDLED_N())
                continue;
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(),
                    PRColors.get(w.meta()-WireDef.BUNDLED_0().meta()).getOreDict(),
                    WireDef.oreDictDefinitionBundled(),
                    Item.silk
                    ));
        }

        /** Framed Wiring **/
        for (WireDef w : WireDef.VALID_WIRE())
            if (w.hasFramedForm())
                // Regular to framed
                GameRegistry.addRecipe(new ShapedOreNBTRecipe(w.getFramedItemStack(),
                        "sss",
                        "sis",
                        "sss",
                        'i', w.getItemStack(),
                        's', ItemMicroPart.create(769, Block.wood.getUnlocalizedName())
                        ).setCheckNBT());
    }

    private static void initPartRecipes()
    {
        /** Wired Plate **/
        GameRegistry.addRecipe(EnumPart.WIREDPLATE.getItemStack(),
                "r",
                "p",
                'r', WireDef.RED_ALLOY().getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
                );

        /** Bundled Plate **/
        GameRegistry.addRecipe(EnumPart.BUNDLEDPLATE.getItemStack(),
                "r",
                "p",
                'r', WireDef.BUNDLED_N().getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
                );
    }
}
