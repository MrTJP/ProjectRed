package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.item.component.BackpackDataComponent;
import net.minecraft.core.component.DataComponentType;
import net.neoforged.neoforge.registries.DeferredHolder;

import static mrtjp.projectred.exploration.ProjectRedExploration.DATA_COMPONENT_TYPES;

public class ExplorationDataComponents {

    public static final String ID_BACKPACK_DATA_COMPONENT = "backpack_data";

    //Data Components
    public static DeferredHolder<DataComponentType<?>, DataComponentType<BackpackDataComponent>> BACKPACK_DATA_COMPONENT;

    public static void register() {
        /* Data Components */
        BACKPACK_DATA_COMPONENT = DATA_COMPONENT_TYPES.register(ID_BACKPACK_DATA_COMPONENT, () ->
                DataComponentType.<BackpackDataComponent>builder()
                        .persistent(BackpackDataComponent.CODEC)
                        .networkSynchronized(BackpackDataComponent.STREAM_CODEC)
                        .build());
    }
}
