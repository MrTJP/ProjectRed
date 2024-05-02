package mrtjp.projectred.core.init;

import mrtjp.projectred.core.ProjectRedCore;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;
import static mrtjp.projectred.core.init.CoreItems.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class CoreCreativeModeTabs {

    public static RegistryObject<CreativeModeTab> CORE_CREATIVE_TAB;

    public static void register() {

        CORE_CREATIVE_TAB = ProjectRedCore.CREATIVE_TABS.register("core",
                () -> CreativeModeTab.builder()
                        .icon(() -> CoreItems.RED_ALLOY_INGOT_ITEM.get().getDefaultInstance())
                        .title(Component.translatable("itemGroup." + MOD_ID))
                        .displayItems((params, output) -> {
                            // Blocks
                            output.accept(ELECTROTINE_GENERATOR_BLOCK.get());

                            // Items
                            output.accept(RED_ALLOY_INGOT_ITEM.get());
                            output.accept(ELECTROTINE_ALLOY_INGOT_ITEM.get());
                            output.accept(ELECTROTINE_DUST_ITEM.get());
                            output.accept(RUBY_ITEM.get());
                            output.accept(SAPPHIRE_ITEM.get());
                            output.accept(PERIDOT_ITEM.get());

                            output.accept(PLATE_ITEM.get());
                            output.accept(CONDUCTIVE_PLATE_ITEM.get());
                            output.accept(WIRED_PLATE_ITEM.get());
                            output.accept(BUNDLED_PLATE_ITEM.get());
                            output.accept(PLATFORMED_PLATE_ITEM.get());
                            output.accept(ANODE_ITEM.get());
                            output.accept(CATHODE_ITEM.get());
                            output.accept(POINTER_ITEM.get());
                            output.accept(SILICON_CHIP_ITEM.get());
                            output.accept(ENERGIZED_SILICON_CHIP_ITEM.get());
                            output.accept(SAND_COAL_COMP_ITEM.get());
                            output.accept(RED_IRON_COMP_ITEM.get());
                            output.accept(ELECTROTINE_IRON_COMP_ITEM.get());
                            output.accept(SILICON_BOULE_ITEM.get());
                            output.accept(SILICON_ITEM.get());
                            output.accept(RED_SILICON_COMP_ITEM.get());
                            output.accept(GLOW_SILICON_COMP_ITEM.get());
                            output.accept(ELECTROTINE_SILICON_COMP_ITEM.get());
                            output.accept(INFUSED_SILICON_ITEM.get());
                            output.accept(ENERGIZED_SILICON_ITEM.get());
                            output.accept(ELECTROTINE_SILICON_ITEM.get());
                            output.accept(COPPER_COIL_ITEM.get());
                            output.accept(IRON_COIL_ITEM.get());
                            output.accept(GOLD_COIL_ITEM.get());
                            output.accept(MOTOR_ITEM.get());
                            output.accept(WOVEN_CLOTH_ITEM.get());
                            output.accept(SAIL_ITEM.get());

                            output.accept(WHITE_ILLUMAR_ITEM.get());
                            output.accept(ORANGE_ILLUMAR_ITEM.get());
                            output.accept(MAGENTA_ILLUMAR_ITEM.get());
                            output.accept(LIGHT_BLUE_ILLUMAR_ITEM.get());
                            output.accept(YELLOW_ILLUMAR_ITEM.get());
                            output.accept(LIME_ILLUMAR_ITEM.get());
                            output.accept(PINK_ILLUMAR_ITEM.get());
                            output.accept(GRAY_ILLUMAR_ITEM.get());
                            output.accept(LIGHT_GRAY_ILLUMAR_ITEM.get());
                            output.accept(CYAN_ILLUMAR_ITEM.get());
                            output.accept(PURPLE_ILLUMAR_ITEM.get());
                            output.accept(BLUE_ILLUMAR_ITEM.get());
                            output.accept(BROWN_ILLUMAR_ITEM.get());
                            output.accept(GREEN_ILLUMAR_ITEM.get());
                            output.accept(RED_ILLUMAR_ITEM.get());
                            output.accept(BLACK_ILLUMAR_ITEM.get());

                            output.accept(DRAW_PLATE_ITEM.get());
                            output.accept(SCREWDRIVER_ITEM.get());
                            output.accept(MULTIMETER_ITEM.get());
                        })
                        .build());
    }
}
