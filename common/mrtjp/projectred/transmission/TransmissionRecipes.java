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
        GameRegistry.addRecipe(WireDefs.RED_ALLOY().getItemStack(12),
                " r ",
                " r ",
                " r ",
                'r', EnumPart.REDINGOT.getItemStack()
                );

        /** Insulated Wires **/
        for (int i = 0; i < WireDefs.INSULATED_WIRE().length; i++)
        {
            WireDef w = WireDefs.INSULATED_WIRE()[i];
            GameRegistry.addRecipe(w.getItemStack(12),
                    "WrW",
                    "WrW",
                    "WrW",
                    'W', new ItemStack(Block.cloth, 1, PRColors.get(i).woolId()),
                    'r', EnumPart.REDINGOT.getItemStack()
                    );
        }
        for (WireDef w : WireDefs.INSULATED_WIRE()) // Recoloring
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(),
                    PRColors.get(w.meta()-WireDefs.INSULATED_0().meta()).getOreDict(),
                    WireDefs.oreDictDefinitionInsulated(),
                    Item.silk
                    ));
        for (WireDef w : WireDefs.INSULATED_WIRE()) // Framed Recoloring
            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(w.getFramedItemStack(),
                    PRColors.get(w.meta()-WireDefs.INSULATED_0().meta()).getOreDict(),
                    WireDefs.oreDictDefinitionInsFramed(),
                    ItemMicroPart.create(769, Block.wood.getUnlocalizedName())
                    ).setCheckNBT());


        /** Bundled Cables **/
        GameRegistry.addRecipe(new ShapedOreRecipe(WireDefs.BUNDLED_N().getItemStack(),
                "SWS",
                "WWW",
                "SWS",
                'S', Item.silk,
                'W', WireDefs.oreDictDefinitionInsulated()
                ));
        for (WireDef w : WireDefs.BUNDLED_WIRE()) // recoloring
        {
            if (w == WireDefs.BUNDLED_N())
                continue;
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack(),
                    PRColors.get(w.meta()-WireDefs.BUNDLED_0().meta()).getOreDict(),
                    WireDefs.oreDictDefinitionBundled(),
                    Item.silk
                    ));
        }

        /** Framed Wiring **/
        for (WireDef w : WireDefs.VALID_WIRE())
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
                'r', WireDefs.RED_ALLOY().getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
                );

        /** Bundled Plate **/
        GameRegistry.addRecipe(EnumPart.BUNDLEDPLATE.getItemStack(),
                "r",
                "p",
                'r', WireDefs.BUNDLED_N().getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
                );
    }
}
