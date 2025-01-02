package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import mrtjp.projectred.expansion.TubeType;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.ItemStack;

import java.util.function.Supplier;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;
import static mrtjp.projectred.expansion.init.ExpansionItems.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionCreativeModeTabs {

    public static Supplier<CreativeModeTab> EXPANSION_CREATIVE_TAB;

    public static void register() {

        EXPANSION_CREATIVE_TAB = ProjectRedExpansion.CREATIVE_TABS.register("expansion",
                () -> CreativeModeTab.builder()
                        .icon(() -> new ItemStack(ExpansionBlocks.PROJECT_BENCH_BLOCK.get()))
                        .title(Component.translatable("itemGroup." + MOD_ID))
                        .displayItems((params, output) -> {

                            // Blocks
                            output.accept(FRAME_BLOCK.get());
                            output.accept(PROJECT_BENCH_BLOCK.get());
                            output.accept(BATTERY_BOX_BLOCK.get());
                            output.accept(CHARGING_BENCH_BLOCK.get());
                            output.accept(AUTO_CRAFTER_BLOCK.get());
                            output.accept(FIRE_STARTER_BLOCK.get());
                            output.accept(FRAME_MOTOR_BLOCK.get());
                            output.accept(FRAME_ACTUATOR_BLOCK.get());
                            output.accept(TRANSPOSER_BLOCK.get());
                            output.accept(BLOCK_BREAKER_BLOCK.get());
                            output.accept(DEPLOYER_BLOCK.get());

                            // Parts
                            for (var type : TubeType.values()) {
                                output.accept(type.getItem());
                            }

                            // Items
                            output.accept(RECIPE_PLAN_ITEM.get());
                            output.accept(BATTERY_ITEM.get());
                            output.accept(EMPTY_BATTERY_ITEM.get());
                            output.accept(ELECTRIC_SCREWDRIVER_ITEM.get());

                        }).build());
    }
}
