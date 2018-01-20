package mrtjp.projectred.archive

import java.lang.{Character => JC}

import codechicken.lib.colour.EnumColour
import mrtjp.projectred.ProjectRedFabrication._
import mrtjp.projectred.core.PartDefs
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack

object FabricationRecipes {
    var dumper: RecipeDumper = _

    def initRecipes() {
        dumper = new RecipeDumper("fabrication")

        //IC Workbench
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(icBlock, 1, 0),
            "iii", "www", "www",
            'i': JC, "blockIron",
            'w': JC, "plankWood")
        ).setJsonName("ic_workbench")

        //IC Printer
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(icBlock, 1, 1),
            "ggg", "oeo", "iwi",
            'g': JC, new ItemStack(Blocks.STAINED_GLASS, 1, EnumColour.LIGHT_BLUE.getWoolMeta),
            'o': JC, Blocks.OBSIDIAN,
            'e': JC, "gemDiamond",
            'i': JC, "ingotIron",
            'w': JC, "plankWood")
        ).setJsonName("ic_printer")

        //IC Blueprint
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(itemICBlueprint),
            "pbp", "brb", "pbp",
            'p': JC, Items.PAPER,
            'b': JC, "dyeBlue",
            'r': JC, Items.REDSTONE)
        ).setJsonName("ic_blueprint")

        //IC Chip
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(itemICChip),
            "ggg", "qdq", "ggg",
            'g': JC, "nuggetGold",
            'q': JC, PartDefs.PLATE.makeStack,
            'd': JC, PartDefs.SILICON.makeStack
        )).setJsonName("ic_chip")
        dumper.dump()
    }
}
