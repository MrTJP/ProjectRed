package mrtjp.projectred.transmission

import codechicken.microblock.ItemMicroPart
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.core.{PartDefs, ShapedOreNBTRecipe, ShapelessOreNBTRecipe}
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.ShapedOreRecipe
import net.minecraftforge.oredict.ShapelessOreRecipe
import mrtjp.projectred.core.libmc.PRColors
import net.minecraft.init.{Items, Blocks}

object TransmissionRecipes
{
    def initTransmissionRecipes()
    {
        initWireRecipes()
        initPartRecipes()
    }

    private def initWireRecipes()
    {
        GameRegistry.addRecipe(WireDef.RED_ALLOY.makeStack(12),
            " r ", " r ", " r ",
            'r':Character, PartDefs.REDINGOT.makeStack)

        for (w <- WireDef.INSULATED_WIRES)
            GameRegistry.addRecipe(w.makeStack(12),
                "WrW", "WrW", "WrW",
                'W':Character, new ItemStack(Blocks.wool, 1, PRColors.get(w.meta-WireDef.INSULATED_0.meta).woolId),
                'r':Character, PartDefs.REDINGOT.makeStack)

        for (w <- WireDef.INSULATED_WIRES)
            GameRegistry.addRecipe(new ShapelessOreRecipe(
                w.makeStack, PRColors.get(w.meta-WireDef.INSULATED_0.meta).getOreDict,
                WireDef.oreDictDefinitionInsulated, Items.string))

        for (w <- WireDef.INSULATED_WIRES)
            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(
                w.makeFramedStack, PRColors.get(w.meta - WireDef.INSULATED_0.meta).getOreDict,
                WireDef.oreDictDefinitionInsFramed, ItemMicroPart.create(769, Blocks.log.getUnlocalizedName)).setCheckNBT())

        GameRegistry.addRecipe(new ShapedOreRecipe(WireDef.BUNDLED_N.makeStack,
            "SWS", "WWW", "SWS",
            'S':Character, Items.string,
            'W':Character, WireDef.oreDictDefinitionInsulated))

        for (w <- WireDef.BUNDLED_WIRES) if (w != WireDef.BUNDLED_N)
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.makeStack, PRColors.get(w.meta-WireDef.BUNDLED_0.meta).getOreDict,
                WireDef.oreDictDefinitionBundled, Items.string))

        for (w <- WireDef.values) if (w.hasFramedForm)
            GameRegistry.addRecipe(new ShapedOreNBTRecipe(w.makeFramedStack,
                "sss", "sis", "sss",
                'i':Character, w.makeStack,
                's':Character, ItemMicroPart.create(769, Blocks.log.getUnlocalizedName)).setCheckNBT())
    }

    private def initPartRecipes()
    {
        GameRegistry.addRecipe(PartDefs.WIREDPLATE.makeStack,
            "r", "p",
            'r':Character, WireDef.RED_ALLOY.makeStack,
            'p':Character, PartDefs.PLATE.makeStack)

        GameRegistry.addRecipe(PartDefs.BUNDLEDPLATE.makeStack,
            "r", "p",
            'r':Character, WireDef.BUNDLED_N.makeStack,
            'p':Character, PartDefs.PLATE.makeStack)
    }
}