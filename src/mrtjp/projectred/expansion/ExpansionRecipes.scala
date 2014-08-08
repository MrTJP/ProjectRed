package mrtjp.projectred.expansion

import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.init.{Items, Blocks}
import net.minecraft.item.ItemStack

import java.lang.{Character => JC}

object ExpansionRecipes
{
    def initRecipes()
    {
        //FurnaceRecipeLib.init()
        initMachineRecipes()
        initMiscRecipes()
    }

    private def initMiscRecipes()
    {
    }

    private def initMachineRecipes()
    {
        //Router controller
        GameRegistry.addRecipe(new ItemStack(ProjectRedExpansion.machine2, 1, 0),
            "w w",
            "ioi",
            "sss",
            'w':JC, Blocks.planks,
            'i':JC, Items.iron_ingot,
            'o':JC, Blocks.obsidian,
            's':JC, Blocks.cobblestone
        )
    }
}