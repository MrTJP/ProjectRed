package mrtjp.projectred.transmission

import java.lang.{Character => JC}

import codechicken.lib.colour.EnumColour
import mrtjp.projectred.core.libmc.recipe._
import mrtjp.projectred.core.{Configurator, PartDefs}
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.registry.GameRegistry
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
                'W':Character, new ItemStack(Blocks.WOOL, 1, EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getWoolDamage),
                'r':Character, PartDefs.oreDictDefinitionRedIngot))

        for (w <- WireDef.INSULATED_WIRES)
            GameRegistry.addRecipe(new ShapelessOreRecipe(
                w.makeStack, EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getOreDictionaryName,
                WireDef.oreDictDefinitionInsulated, Items.STRING))

        for (w <- WireDef.INSULATED_WIRES)
            (RecipeLib.newShapelessBuilder
                += new OreIn(EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getOreDictionaryName)
                += new OreIn(WireDef.oreDictDefinitionInsFramed)
                += new MicroIn(MicroIn.edge, MicroIn.eight, Blocks.LOG)
                += new ItemOut(w.makeFramedStack)).registerResult()

        GameRegistry.addRecipe(new ShapedOreRecipe(WireDef.BUNDLED_N.makeStack,
            "SWS", "WWW", "SWS",
            'S':Character, Items.STRING,
            'W':Character, WireDef.oreDictDefinitionInsulated))

        for (w <- WireDef.BUNDLED_WIRES) if (w != WireDef.BUNDLED_N)
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.makeStack, EnumColour.values()(w.meta-WireDef.BUNDLED_0.meta).getOreDictionaryName,
                WireDef.oreDictDefinitionBundled, Items.STRING))

        for (w <- WireDef.values) if (w.hasFramedForm)
            (RecipeLib.newShapedBuilder <-> "sss"+"sis"+"sss"
                += new ItemIn(w.makeStack) to "i"
                += (if (Configurator.simpleFramedWireRecipe) new OreIn("stickWood") to "s"
                    else new MicroIn(MicroIn.edge, MicroIn.eight, Blocks.LOG) to "s")
                += new ItemOut(w.makeFramedStack)).registerResult()

        GameRegistry.addRecipe(new ShapedOreRecipe(WireDef.POWER_LOWLOAD.makeStack(12),
            "bib","yiy", "bib",
            'b':JC, new ItemStack(Blocks.WOOL, 1, EnumColour.BLUE.getWoolDamage),
            'i':JC, "ingotElectrotineAlloy",
            'y':JC, new ItemStack(Blocks.WOOL, 1, EnumColour.YELLOW.getWoolDamage)
        ))
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