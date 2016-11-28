/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.core.libmc.recipe

import net.minecraftforge.fml.common.registry.GameRegistry

class SmeltingRecipeBuilder extends RecipeBuilder
{
    private var xp = 0.0f
    def setXP(i:Float):this.type = {xp = i; this}

    def registerResults()
    {
        compute()
        inResult.head.matchingInputs.foreach(
            GameRegistry.addSmelting(_, outResult.head.createOutput, xp)
        )
    }
}