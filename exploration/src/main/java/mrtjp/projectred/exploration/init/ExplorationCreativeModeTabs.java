package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.ProjectRedExploration;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.ItemStack;

import java.util.function.Supplier;

import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationItems.*;

public class ExplorationCreativeModeTabs {

    public static Supplier<CreativeModeTab> EXPLORATION_CREATIVE_TAB;

    public static void register() {

        EXPLORATION_CREATIVE_TAB = ProjectRedExploration.CREATIVE_TABS.register("expansion",
                () -> CreativeModeTab.builder()
                        .icon(() -> new ItemStack(MARBLE_BRICK_BLOCK.get()))
                        .title(Component.translatable("itemGroup." + ProjectRedExploration.MOD_ID))
                        .displayItems((params, output) -> {

                            // Keep in sync with ExplorationBlocks and ExplorationItems

                            /* Blocks */

                            // Ores
                            output.accept(RUBY_ORE_BLOCK.get());
                            output.accept(DEEPSLATE_RUBY_ORE_BLOCK.get());
                            output.accept(SAPPHIRE_ORE_BLOCK.get());
                            output.accept(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get());
                            output.accept(PERIDOT_ORE_BLOCK.get());
                            output.accept(DEEPSLATE_PERIDOT_ORE_BLOCK.get());
                            output.accept(TIN_ORE_BLOCK.get());
                            output.accept(DEEPSLATE_TIN_ORE_BLOCK.get());
                            output.accept(SILVER_ORE_BLOCK.get());
                            output.accept(DEEPSLATE_SILVER_ORE_BLOCK.get());
                            output.accept(ELECTROTINE_ORE_BLOCK.get());
                            output.accept(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get());

                            // Decorative blocks
                            output.accept(MARBLE_BLOCK.get());
                            output.accept(MARBLE_BRICK_BLOCK.get());
                            output.accept(BASALT_BLOCK.get());
                            output.accept(BASALT_COBBLE_BLOCK.get());
                            output.accept(BASALT_BRICK_BLOCK.get());
                            output.accept(RUBY_BLOCK.get());
                            output.accept(SAPPHIRE_BLOCK.get());
                            output.accept(PERIDOT_BLOCK.get());
                            output.accept(ELECTROTINE_BLOCK.get());
                            output.accept(RAW_TIN_BLOCK.get());
                            output.accept(RAW_SILVER_BLOCK.get());
                            output.accept(TIN_BLOCK.get());
                            output.accept(SILVER_BLOCK.get());

                            // Walls
                            output.accept(MARBLE_WALL.get());
                            output.accept(MARBLE_BRICK_WALL.get());
                            output.accept(BASALT_WALL.get());
                            output.accept(BASALT_COBBLE_WALL.get());
                            output.accept(BASALT_BRICK_WALL.get());
                            output.accept(RUBY_BLOCK_WALL.get());
                            output.accept(SAPPHIRE_BLOCK_WALL.get());
                            output.accept(PERIDOT_BLOCK_WALL.get());
                            output.accept(ELECTROTINE_BLOCK_WALL.get());

                            /* Items */

                            // Ingots / dusts / gems
                            output.accept(RAW_TIN_ITEM.get());
                            output.accept(TIN_INGOT_ITEM.get());
                            output.accept(RAW_SILVER_ITEM.get());
                            output.accept(SILVER_INGOT_ITEM.get());

                            output.accept(WOOL_GIN.get());
                            output.accept(ATHAME.get());

                            // Tools
                            output.accept(RUBY_AXE.get());
                            output.accept(SAPPHIRE_AXE.get());
                            output.accept(PERIDOT_AXE.get());

                            output.accept(RUBY_PICKAXE.get());
                            output.accept(SAPPHIRE_PICKAXE.get());
                            output.accept(PERIDOT_PICKAXE.get());

                            output.accept(RUBY_SHOVEL.get());
                            output.accept(SAPPHIRE_SHOVEL.get());
                            output.accept(PERIDOT_SHOVEL.get());

                            output.accept(RUBY_HOE.get());
                            output.accept(SAPPHIRE_HOE.get());
                            output.accept(PERIDOT_HOE.get());

                            output.accept(RUBY_SWORD.get());
                            output.accept(SAPPHIRE_SWORD.get());
                            output.accept(PERIDOT_SWORD.get());

                            output.accept(GOLD_SAW.get());
                            output.accept(RUBY_SAW.get());
                            output.accept(SAPPHIRE_SAW.get());
                            output.accept(PERIDOT_SAW.get());

                            output.accept(WOOD_SICKLE.get());
                            output.accept(STONE_SICKLE.get());
                            output.accept(IRON_SICKLE.get());
                            output.accept(GOLD_SICKLE.get());
                            output.accept(DIAMOND_SICKLE.get());
                            output.accept(RUBY_SICKLE.get());
                            output.accept(SAPPHIRE_SICKLE.get());
                            output.accept(PERIDOT_SICKLE.get());

                            // Armor
                            output.accept(RUBY_HELMET.get());
                            output.accept(SAPPHIRE_HELMET.get());
                            output.accept(PERIDOT_HELMET.get());

                            output.accept(RUBY_CHESTPLATE.get());
                            output.accept(SAPPHIRE_CHESTPLATE.get());
                            output.accept(PERIDOT_CHESTPLATE.get());

                            output.accept(RUBY_LEGGINGS.get());
                            output.accept(SAPPHIRE_LEGGINGS.get());
                            output.accept(PERIDOT_LEGGINGS.get());

                            output.accept(RUBY_BOOTS.get());
                            output.accept(SAPPHIRE_BOOTS.get());
                            output.accept(PERIDOT_BOOTS.get());

                            //Backpacks
                            output.accept(WHITE_BACKPACK.get());
                            output.accept(ORANGE_BACKPACK.get());
                            output.accept(MAGENTA_BACKPACK.get());
                            output.accept(LIGHT_BLUE_BACKPACK.get());
                            output.accept(YELLOW_BACKPACK.get());
                            output.accept(LIME_BACKPACK.get());
                            output.accept(PINK_BACKPACK.get());
                            output.accept(GRAY_BACKPACK.get());
                            output.accept(LIGHT_GRAY_BACKPACK.get());
                            output.accept(CYAN_BACKPACK.get());
                            output.accept(PURPLE_BACKPACK.get());
                            output.accept(BLUE_BACKPACK.get());
                            output.accept(BROWN_BACKPACK.get());
                            output.accept(GREEN_BACKPACK.get());
                            output.accept(RED_BACKPACK.get());
                            output.accept(BLACK_BACKPACK.get());

                        }).build());


    }
}
