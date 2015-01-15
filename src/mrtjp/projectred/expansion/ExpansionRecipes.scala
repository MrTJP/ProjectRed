package mrtjp.projectred.expansion

import java.lang.{Character => JC}

import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.ProjectRedExpansion._
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.ShapedOreRecipe

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
        //Block Breaker
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 0),
            "sas", "sps", "srs",
            's':JC, Blocks.cobblestone,
            'a':JC, Items.iron_pickaxe,
            'p':JC, Blocks.piston,
            'r':JC, Items.redstone
        ))

        //Block Breaker
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 1),
            "www", "sps", "srs",
            'w':JC, "plankWood",
            's':JC, Blocks.cobblestone,
            'p':JC, Blocks.piston,
            'r':JC, Items.redstone
        ))
    }
}