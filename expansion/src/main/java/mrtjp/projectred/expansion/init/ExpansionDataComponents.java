package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.item.BatteryBoxStorageComponent;
import mrtjp.projectred.expansion.item.RecipePlanComponent;
import net.minecraft.core.component.DataComponentType;
import net.neoforged.neoforge.registries.DeferredHolder;

import static mrtjp.projectred.expansion.ProjectRedExpansion.DATA_COMPONENT_TYPES;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionDataComponents {

    public static final String ID_BATTERY_BOX_STORAGE = "battery_box_storage";
    public static final String ID_RECIPE_PLAN = "recipe_plan";

    public static DeferredHolder<DataComponentType<?>, DataComponentType<BatteryBoxStorageComponent>> BATTERY_BOX_STORAGE_COMPONENT_TYPE;
    public static DeferredHolder<DataComponentType<?>, DataComponentType<RecipePlanComponent>> RECIPE_PLAN_COMPONENT_TYPE;

    public static void register() {
        BATTERY_BOX_STORAGE_COMPONENT_TYPE = DATA_COMPONENT_TYPES.register(ID_BATTERY_BOX_STORAGE,
                () -> DataComponentType.<BatteryBoxStorageComponent>builder()
                        .persistent(BatteryBoxStorageComponent.CODEC)
                        .networkSynchronized(BatteryBoxStorageComponent.STREAM_CODEC)
                        .build());

        RECIPE_PLAN_COMPONENT_TYPE = DATA_COMPONENT_TYPES.register(ID_RECIPE_PLAN,
                () -> DataComponentType.<RecipePlanComponent>builder()
                        .persistent(RecipePlanComponent.CODEC)
                        .networkSynchronized(RecipePlanComponent.STREAM_CODEC)
                        .build());
    }
}
