package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.item.crafting.BackpackDyeRecipe;
import net.minecraft.world.item.crafting.SimpleRecipeSerializer;

import static mrtjp.projectred.exploration.ProjectRedExploration.RECIPE_SERIALIZERS;

public class ExplorationRecipeSerializers {

    public static final String ID_BACKPACK_DYE = "backpack_dye";

    public static void register() {

        RECIPE_SERIALIZERS.register(ID_BACKPACK_DYE, () -> new SimpleRecipeSerializer<>(BackpackDyeRecipe::new));
    }
}
