package mrtjp.projectred.integration.init;

import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.ProjectRedIntegration;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;

import java.util.function.Supplier;

@SuppressWarnings("NotNullFieldNotInitialized")
public class IntegrationCreativeModeTabs {

    public static Supplier<CreativeModeTab> INTEGRATION_CREATIVE_TAB;

    public static void register() {
        INTEGRATION_CREATIVE_TAB = ProjectRedIntegration.CREATIVE_TABS.register("integration",
                () -> CreativeModeTab.builder()
                        .icon(GateType.OR::makeStack)
                        .title(Component.translatable("itemGroup." + ProjectRedIntegration.MOD_ID))
                        .displayItems((params, output) -> {
                            // Gates
                            for (GateType type : GateType.values()) {
                                if (!type.isExternalGate())
                                    output.accept(type.makeStack());
                            }
                        })
                        .build());
    }

}
