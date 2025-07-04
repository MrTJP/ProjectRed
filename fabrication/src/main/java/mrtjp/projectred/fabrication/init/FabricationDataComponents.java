package mrtjp.projectred.fabrication.init;

import mrtjp.projectred.fabrication.item.component.BlueprintDataComponent;
import mrtjp.projectred.fabrication.item.component.ICDataComponent;
import net.minecraft.core.component.DataComponentType;
import net.neoforged.neoforge.registries.DeferredHolder;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.DATA_COMPONENT_TYPES;

@SuppressWarnings("NotNullFieldNotInitialized")
public class FabricationDataComponents {

    public static final String ID_BLUEPRINT_DATA = "blueprint_data";
    public static final String ID_IC_DATA = "ic_data";

    public static DeferredHolder<DataComponentType<?>, DataComponentType<BlueprintDataComponent>> BLUEPRINT_DATA_COMPONENT_TYPE;
    public static DeferredHolder<DataComponentType<?>, DataComponentType<ICDataComponent>> IC_DATA_COMPONENT_TYPE;

    public static void register() {
        BLUEPRINT_DATA_COMPONENT_TYPE = DATA_COMPONENT_TYPES.register(ID_BLUEPRINT_DATA,
                () -> DataComponentType.<BlueprintDataComponent>builder()
                        .persistent(BlueprintDataComponent.CODEC)
                        .networkSynchronized(BlueprintDataComponent.STREAM_CODEC)
                        .build());

        IC_DATA_COMPONENT_TYPE = DATA_COMPONENT_TYPES.register(ID_IC_DATA,
                () -> DataComponentType.<ICDataComponent>builder()
                        .persistent(ICDataComponent.CODEC)
                        .networkSynchronized(ICDataComponent.STREAM_CODEC)
                        .build());
    }
}
