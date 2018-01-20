package mrtjp.projectred.archive

import java.lang.{Character => JC}

import codechicken.lib.colour.EnumColour
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.illumination.{LightFactoryCage, LightFactoryFallout, LightFactoryFixture, LightFactoryLantern}
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack

object IlluminationRecipes
{
    var dumper:RecipeDumper = _
    def initRecipes()
    {
        dumper = new RecipeDumper("illumination")
        initLighting()
        dumper.dump()
    }

    private def initLighting()
    {
        /** Lamps **/
        for (i <- 0 until 16)
        {
            dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedIllumination.blockLamp, 1, i),
                "gIg",
                "gIg",
                "gtg",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                't':JC, "dustRedstone"
            )).setJsonName(s"lamp\\${EnumColour.fromWoolMeta(i).getName}_lamp")

            dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedIllumination.blockLamp, 1, i+16),
                "gIg",
                "gIg",
                "gtg",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                't':JC, Blocks.REDSTONE_TORCH
            )).setJsonName(s"lamp\\${EnumColour.fromWoolMeta(i).getName}_lamp_inverted")
        }

        /** Lanterns **/
        for (i <- 0 until 16)
        {
            dumper.addRecipe(new ShapedOreRecipe(LightFactoryLantern.makeStack(i),
                "PNP",
                "GIG",
                "PRP",
                'P':JC, PartDefs.PLATE.makeStack,
                'N':JC, "nuggetGold",
                'G':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'R':JC, "dustRedstone"
            )).setJsonName(s"lantern\\${EnumColour.fromWoolMeta(i).getName}_lantern")

            dumper.addRecipe(new ShapedOreRecipe(LightFactoryLantern.makeInvStack(i),
                "PNP",
                "GIG",
                "PRP",
                'P':JC, PartDefs.PLATE.makeStack,
                'N':JC, "nuggetGold",
                'G':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'R':JC, Blocks.REDSTONE_TORCH
            )).setJsonName(s"lantern\\${EnumColour.fromWoolMeta(i).getName}_lantern_inverted")
        }

        /** Buttons **/
        for (i <- 0 until 16)
        {
            dumper.addRecipe(new ShapelessOreRecipe(new ItemStack(ProjectRedIllumination.itemPartIllumarButton, 1, i),
                Blocks.STONE_BUTTON,
                PartDefs.ILLUMARS.toSeq(i).makeStack,
                PartDefs.ILLUMARS.toSeq(i).makeStack)
            ).setJsonName(s"button\\${EnumColour.fromWoolMeta(i).getName}_button")
            dumper.addRecipe(new ShapelessOreRecipe(new ItemStack(ProjectRedIllumination.itemPartIllumarFButton, 1, i),
                new ItemStack(ProjectRedIllumination.itemPartIllumarButton, 1, i),
                Blocks.REDSTONE_TORCH)
            ).setJsonName(s"button\\${EnumColour.fromWoolMeta(i).getName}_button_inverted")
        }

        /** Fallout Lights **/
        for (i <- 0 until 16)
        {
            dumper.addRecipe(new ShapedOreRecipe(LightFactoryFallout.makeStack(i),
                "CCC", "CIC", "NPN",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'N':JC, "nuggetGold",
                'P':JC, PartDefs.CONDUCTIVEPLATE.makeStack
            )).setJsonName(s"fallout\\${EnumColour.fromWoolMeta(i).getName}_fallout")

            dumper.addRecipe(new ShapedOreRecipe(LightFactoryFallout.makeInvStack(i),
                "CCC", "CIC", "NPN",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'N':JC, "nuggetGold",
                'P':JC, PartDefs.CATHODE.makeStack
            )).setJsonName(s"fallout\\${EnumColour.fromWoolMeta(i).getName}_fallout_inverted")
        }

        /** Cage Lamps **/
        for (i <- 0 until 16)
        {
            dumper.addRecipe(LightFactoryCage.makeStack(i),
                " C ", "CIC", "pPp",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CONDUCTIVEPLATE.makeStack
            ).setJsonName(s"cage\\${EnumColour.fromWoolMeta(i).getName}_cage")

            dumper.addRecipe(LightFactoryCage.makeInvStack(i),
                " C ", "CIC", "pPp",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CATHODE.makeStack
            ).setJsonName(s"cage\\${EnumColour.fromWoolMeta(i).getName}_cage_inverted")
        }

        /** Fixtures **/
        for (i <- 0 until 16)
        {
            dumper.addRecipe(new ShapedOreRecipe(LightFactoryFixture.makeStack(i),
                "ggg", "gIg", "pPp",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CONDUCTIVEPLATE.makeStack
            )).setJsonName(s"fixture\\${EnumColour.fromWoolMeta(i).getName}_fixture")
            dumper.addRecipe(new ShapedOreRecipe(LightFactoryFixture.makeInvStack(i),
                "ggg", "gIg", "pPp",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CATHODE.makeStack
            )).setJsonName(s"fixture\\${EnumColour.fromWoolMeta(i).getName}_fixture_inverted")
        }
    }
}
