package mrtjp.projectred.transmission.init;

import mrtjp.projectred.transmission.ProjectRedTransmission;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraftforge.registries.RegistryObject;

@SuppressWarnings("NotNullFieldNotInitialized")
public class TransmissionCreativeModeTabs {

    public static RegistryObject<CreativeModeTab> TRANSMISSION_CREATIVE_TAB;

    public static void register() {
        TRANSMISSION_CREATIVE_TAB = ProjectRedTransmission.CREATIVE_TABS.register("transmission",
                () -> CreativeModeTab.builder()
                        .icon(WireType.RED_ALLOY::makeStack)
                        .title(Component.translatable("itemGroup." + ProjectRedTransmission.MOD_ID))
                        .displayItems((params, output) -> {

                            // Wires
                            for (WireType type : WireType.values()) {
                                output.accept(type.makeStack());
                            }
                        })
                        .build());
    }
}
