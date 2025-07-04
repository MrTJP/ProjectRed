package mrtjp.projectred.fabrication.init;

import mrtjp.projectred.integration.ProjectRedIntegration;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.ItemStack;

import java.util.function.Supplier;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;
import static mrtjp.projectred.fabrication.init.FabricationItems.*;

public class FabricationCreativeModeTab {

    public static Supplier<CreativeModeTab> FABRICATION_CREATIVE_TAB;

    public static void register() {
        FABRICATION_CREATIVE_TAB = ProjectRedIntegration.CREATIVE_TABS.register("fabrication",
                () -> CreativeModeTab.builder()
                        .icon(() -> new ItemStack(IC_WORKBENCH_BLOCK.get()))
                        .title(Component.translatable("itemGroup." + MOD_ID))
                        .displayItems((params, output) -> {
                            // Blocks
                            output.accept(IC_WORKBENCH_BLOCK.get());
                            output.accept(PLOTTING_TABLE_BLOCK.get());
                            output.accept(LITHOGRAPHY_TABLE_BLOCK.get());
                            output.accept(PACKAGING_TABLE_BLOCK.get());

                            // Items
                            output.accept(IC_BLUEPRINT_ITEM.get());
                            output.accept(BLANK_PHOTOMASK_ITEM.get());
                            output.accept(PHOTOMASK_SET_ITEM.get());
                            output.accept(ROUGH_SILICON_WAFER_ITEM.get());
                            output.accept(VALID_DIE_ITEM.get());
                            output.accept(INVALID_DIE_ITEM.get());
                        })
                        .build());
    }

}
