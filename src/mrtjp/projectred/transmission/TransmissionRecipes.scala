package mrtjp.projectred.transmission

import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.core.color.Colors
import mrtjp.projectred.core.libmc.recipe._
import mrtjp.projectred.core.{Configurator, PartDefs}
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.{ShapedOreRecipe, ShapelessOreRecipe}

object TransmissionRecipes
{
    def initTransmissionRecipes()
    {
        WireDef.initOreDict()
        initWireRecipes()
        initPartRecipes()
    }

    private def initWireRecipes()
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(WireDef.RED_ALLOY.makeStack(12),
            " r ", " r ", " r ",
            'r':Character, PartDefs.oreDictDefinitionRedIngot))

        for (w <- WireDef.INSULATED_WIRES)
            GameRegistry.addRecipe(new ShapedOreRecipe(w.makeStack(12),
                "WrW", "WrW", "WrW",
                'W':Character, new ItemStack(Blocks.wool, 1, Colors.get(w.meta-WireDef.INSULATED_0.meta).woolId),
                'r':Character, PartDefs.oreDictDefinitionRedIngot))

        for (w <- WireDef.INSULATED_WIRES)
            GameRegistry.addRecipe(new ShapelessOreRecipe(
                w.makeStack, Colors.get(w.meta-WireDef.INSULATED_0.meta).getOreDict,
                WireDef.oreDictDefinitionInsulated, Items.string))

        for (w <- WireDef.INSULATED_WIRES)
            (RecipeLib.newShapelessBuilder
                += new OreIn(Colors.get(w.meta - WireDef.INSULATED_0.meta).getOreDict)
                += new OreIn(WireDef.oreDictDefinitionInsFramed)
                += new MicroIn(MicroIn.edge, MicroIn.eight, Blocks.log)
                += new ItemOut(w.makeFramedStack)).registerResult()

        GameRegistry.addRecipe(new ShapedOreRecipe(WireDef.BUNDLED_N.makeStack,
            "SWS", "WWW", "SWS",
            'S':Character, Items.string,
            'W':Character, WireDef.oreDictDefinitionInsulated))

        for (w <- WireDef.BUNDLED_WIRES) if (w != WireDef.BUNDLED_N)
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.makeStack, Colors.get(w.meta-WireDef.BUNDLED_0.meta).getOreDict,
                WireDef.oreDictDefinitionBundled, Items.string))

        for (w <- WireDef.values) if (w.hasFramedForm)
            (RecipeLib.newShapedBuilder <-> "sss"+"sis"+"sss"
                += new ItemIn(w.makeStack) to "i"
                += (if (Configurator.simpleFramedWireRecipe) new OreIn("stickWood") to "s"
                    else new MicroIn(MicroIn.edge, MicroIn.eight, Blocks.log) to "s")
                += new ItemOut(w.makeFramedStack)).registerResult()
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