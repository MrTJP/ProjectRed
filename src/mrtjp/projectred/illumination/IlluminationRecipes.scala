package mrtjp.projectred.illumination

import java.lang.{Character => JC}

import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.PartDefs
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.oredict.ShapedOreRecipe

object IlluminationRecipes
{
    def initRecipes()
    {
        initLighting()
    }

    private def initLighting()
    {
        /** Lamps **/
        for (i <- 0 until 16)
        {
            GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedIllumination.blockLamp, 1, i),
                "gIg",
                "gIg",
                "gtg",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                't':JC, "dustRedstone"
            ))

            GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedIllumination.blockLamp, 1, i+16),
                "gIg",
                "gIg",
                "gtg",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                't':JC, Blocks.REDSTONE_TORCH
            ))
        }

        /** Lanterns **/
        for (i <- 0 until 16)
        {
            GameRegistry.addRecipe(new ShapedOreRecipe(LightFactoryLantern.makeStack(i),
                "PNP",
                "GIG",
                "PRP",
                'P':JC, PartDefs.PLATE.makeStack,
                'N':JC, "nuggetGold",
                'G':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'R':JC, "dustRedstone"
            ))
            GameRegistry.addRecipe(new ShapedOreRecipe(LightFactoryLantern.makeInvStack(i),
                "PNP",
                "GIG",
                "PRP",
                'P':JC, PartDefs.PLATE.makeStack,
                'N':JC, "nuggetGold",
                'G':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'R':JC, Blocks.REDSTONE_TORCH
            ))
        }

        /** Buttons **/
        for (i <- 0 until 16)
        {
            GameRegistry.addShapelessRecipe(new ItemStack(ProjectRedIllumination.itemPartIllumarButton, 1, i),
                Blocks.STONE_BUTTON,
                PartDefs.ILLUMARS.toSeq(i).makeStack,
                PartDefs.ILLUMARS.toSeq(i).makeStack
            )
            GameRegistry.addShapelessRecipe(new ItemStack(ProjectRedIllumination.itemPartIllumarFButton, 1, i),
                new ItemStack(ProjectRedIllumination.itemPartIllumarButton, 1, i),
                Blocks.REDSTONE_TORCH
            )
        }

        /** Fallout Lights **/
        for (i <- 0 until 16)
        {
            GameRegistry.addRecipe(new ShapedOreRecipe(LightFactoryFallout.makeStack(i),
                "CCC", "CIC", "NPN",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'N':JC, "nuggetGold",
                'P':JC, PartDefs.CONDUCTIVEPLATE.makeStack
            ))

            GameRegistry.addRecipe(new ShapedOreRecipe(LightFactoryFallout.makeInvStack(i),
                "CCC", "CIC", "NPN",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'N':JC, "nuggetGold",
                'P':JC, PartDefs.CATHODE.makeStack
            ))
        }

        /** Cage Lamps **/
        for (i <- 0 until 16)
        {
            GameRegistry.addRecipe(LightFactoryCage.makeStack(i),
                " C ", "CIC", "pPp",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CONDUCTIVEPLATE.makeStack
            )

            GameRegistry.addRecipe(LightFactoryCage.makeInvStack(i),
                " C ", "CIC", "pPp",
                'C':JC, Blocks.IRON_BARS,
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CATHODE.makeStack
            )
        }

        /** Fixtures **/
        for (i <- 0 until 16)
        {
            GameRegistry.addRecipe(new ShapedOreRecipe(LightFactoryFixture.makeStack(i),
                "ggg", "gIg", "pPp",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CONDUCTIVEPLATE.makeStack
            ))
            GameRegistry.addRecipe(new ShapedOreRecipe(LightFactoryFixture.makeInvStack(i),
                "ggg", "gIg", "pPp",
                'g':JC, "paneGlassColorless",
                'I':JC, PartDefs.ILLUMARS.toSeq(i).makeStack,
                'p':JC, PartDefs.PLATE.makeStack,
                'P':JC, PartDefs.CATHODE.makeStack
            ))
        }
    }
}
