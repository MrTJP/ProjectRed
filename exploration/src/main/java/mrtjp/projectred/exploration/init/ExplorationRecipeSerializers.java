package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.item.crafting.BackpackDyeRecipe;
import net.minecraft.world.item.crafting.SimpleRecipeSerializer;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.exploration.ProjectRedExploration.RECIPE_SERIALIZERS;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationRecipeSerializers {

    public static final String ID_BACKPACK_DYE = "backpack_dye";

    public static RegistryObject<SimpleRecipeSerializer<?>> BACKPACK_DYE_RECIPE_SERIALIZER;

    public static void register() {

        BACKPACK_DYE_RECIPE_SERIALIZER = RECIPE_SERIALIZERS.register(ID_BACKPACK_DYE, () -> new SimpleRecipeSerializer<>(BackpackDyeRecipe::new));
    }
}
