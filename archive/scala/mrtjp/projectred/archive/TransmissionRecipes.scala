package mrtjp.projectred.archive

import java.lang.{Character => JC}

import codechicken.lib.colour.EnumColour
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.transmission.WireDef
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack

object TransmissionRecipes
{
    var dumper:RecipeDumper = _
    def initTransmissionRecipes()
    {
        dumper = new RecipeDumper("transmission")
        initWireRecipes()
        initPartRecipes()
        dumper.dump()
    }

    private def initWireRecipes()
    {
        dumper.addRecipe(new ShapedOreRecipe(WireDef.RED_ALLOY.makeStack(12),
            " r ", " r ", " r ",
            'r':Character, PartDefs.oreDictDefinitionRedIngot)
        ).setJsonName("red_alloy_wire")

        //Insulated wire
        for (w <- WireDef.INSULATED_WIRES)
            dumper.addRecipe(new ShapedOreRecipe(w.makeStack(12),
                "WrW", "WrW", "WrW",
                'W':Character, new ItemStack(Blocks.WOOL, 1, EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getWoolMeta),
                'r':Character, PartDefs.oreDictDefinitionRedIngot)
            ).setJsonName(s"insulated\\${EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getName}_insulated_wire")

        //Insulated wire recolouring
        for (w <- WireDef.INSULATED_WIRES)
            dumper.addRecipe(new ShapelessOreRecipe(w.makeStack,
                EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getDyeOreName,
                WireDef.oreDictDefinitionInsulated)
            ).setJsonName(s"insulated\\${EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getName}_insulated_wire_re_colour")

        //Framed insulated wire recolouring
        for (w <- WireDef.INSULATED_WIRES)
            dumper.addRecipe(new ShapelessOreRecipe(w.makeFramedStack,
                EnumColour.values()(w.meta-WireDef.INSULATED_0.meta).getDyeOreName,
                WireDef.oreDictDefinitionInsFramed)
            ).setJsonName(s"framed\\${EnumColour.values()(w.meta-WireDef.INSULATED_0.meta)}_framed_wire_re_colour")

        //Bundled cable
        dumper.addRecipe(new ShapedOreRecipe(WireDef.BUNDLED_N.makeStack,
            "SWS", "WWW", "SWS",
            'S':Character, Items.STRING,
            'W':Character, WireDef.oreDictDefinitionInsulated)
        ).setJsonName("bundled_wire")

        //Bundled cable recolouring
        for (w <- WireDef.BUNDLED_WIRES) if (w != WireDef.BUNDLED_N)
            dumper.addRecipe(new ShapelessOreRecipe(w.makeStack, EnumColour.values()(w.meta-WireDef.BUNDLED_0.meta).getDyeOreName,
                WireDef.oreDictDefinitionBundled, Items.STRING)
            ).setJsonName(s"bundled\\${EnumColour.values()(w.meta-WireDef.BUNDLED_0.meta)}_bundled_re_colour")

        //Framed insulated+bundled wire
        for (w <- WireDef.values) if (w.hasFramedForm)
            dumper.addRecipe(new ShapedOreRecipe(w.makeFramedStack,
                "sss", "sis", "sss",
                's':Character, "stickWood",
                'i':Character, w.makeStack)
            ).setJsonName(s"framed\\${w.ordinal}_framed")

        dumper.addRecipe(new ShapedOreRecipe(WireDef.POWER_LOWLOAD.makeStack(12),
            "bib","yiy", "bib",
            'b':JC, new ItemStack(Blocks.WOOL, 1, EnumColour.BLUE.getWoolMeta),
            'i':JC, "ingotElectrotineAlloy",
            'y':JC, new ItemStack(Blocks.WOOL, 1, EnumColour.YELLOW.getWoolMeta))
        ).setJsonName("power_low_load")
    }

    private def initPartRecipes()
    {
        dumper.addRecipe(PartDefs.WIREDPLATE.makeStack,
            "r", "p",
            'r':Character, WireDef.RED_ALLOY.makeStack,
            'p':Character, PartDefs.PLATE.makeStack
        ).setJsonName("wired_plate")

        dumper.addRecipe(PartDefs.BUNDLEDPLATE.makeStack,
            "r", "p",
            'r':Character, WireDef.BUNDLED_N.makeStack,
            'p':Character, PartDefs.PLATE.makeStack
        ).setJsonName("bundled_plate")
    }
}
