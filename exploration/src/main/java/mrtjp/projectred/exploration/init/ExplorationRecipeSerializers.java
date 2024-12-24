package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.item.crafting.BackpackDyeRecipe;
import net.minecraft.world.item.crafting.SimpleCraftingRecipeSerializer;

import java.util.function.Supplier;

import static mrtjp.projectred.exploration.ProjectRedExploration.RECIPE_SERIALIZERS;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationRecipeSerializers {

    public static final String ID_BACKPACK_DYE = "backpack_dye";

    public static Supplier<SimpleCraftingRecipeSerializer<?>> BACKPACK_DYE_RECIPE_SERIALIZER;

    public static void register() {

        BACKPACK_DYE_RECIPE_SERIALIZER = RECIPE_SERIALIZERS.register(ID_BACKPACK_DYE, () -> new SimpleCraftingRecipeSerializer<>(BackpackDyeRecipe::new));
    }
}
