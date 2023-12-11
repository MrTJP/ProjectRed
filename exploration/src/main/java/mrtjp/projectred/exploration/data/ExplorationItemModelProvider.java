package mrtjp.projectred.exploration.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationItems.*;

public class ExplorationItemModelProvider extends ItemModelProvider {

    public ExplorationItemModelProvider(DataGenerator generator, ExistingFileHelper existingFileHelper) {
        super(generator, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Exploration Item Models";
    }

    @Override
    protected void registerModels() {

        /* Block Items */

        // Ore Blocks
        simpleItemBlock(RUBY_ORE_BLOCK.get());
        simpleItemBlock(DEEPSLATE_RUBY_ORE_BLOCK.get());
        simpleItemBlock(SAPPHIRE_ORE_BLOCK.get());
        simpleItemBlock(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get());
        simpleItemBlock(PERIDOT_ORE_BLOCK.get());
        simpleItemBlock(DEEPSLATE_PERIDOT_ORE_BLOCK.get());
        simpleItemBlock(TIN_ORE_BLOCK.get());
        simpleItemBlock(DEEPSLATE_TIN_ORE_BLOCK.get());
        simpleItemBlock(SILVER_ORE_BLOCK.get());
        simpleItemBlock(DEEPSLATE_SILVER_ORE_BLOCK.get());
        simpleItemBlock(ELECTROTINE_ORE_BLOCK.get());
        simpleItemBlock(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get());

        // Decorative Blocks
        simpleItemBlock(MARBLE_BLOCK.get());
        simpleItemBlock(MARBLE_BRICK_BLOCK.get());
        simpleItemBlock(BASALT_BLOCK.get());
        simpleItemBlock(BASALT_COBBLE_BLOCK.get());
        simpleItemBlock(BASALT_BRICK_BLOCK.get());
        simpleItemBlock(RUBY_BLOCK.get());
        simpleItemBlock(SAPPHIRE_BLOCK.get());
        simpleItemBlock(PERIDOT_BLOCK.get());
        simpleItemBlock(ELECTROTINE_BLOCK.get());
        simpleItemBlock(RAW_TIN_BLOCK.get());
        simpleItemBlock(TIN_BLOCK.get());
        simpleItemBlock(RAW_SILVER_BLOCK.get());
        simpleItemBlock(SILVER_BLOCK.get());

        // Walls
        wallItemBlock(MARBLE_WALL.get(), MARBLE_BLOCK.get());
        wallItemBlock(MARBLE_BRICK_WALL.get(), MARBLE_BRICK_BLOCK.get());
        wallItemBlock(BASALT_WALL.get(), BASALT_BLOCK.get());
        wallItemBlock(BASALT_COBBLE_WALL.get(), BASALT_COBBLE_BLOCK.get());
        wallItemBlock(BASALT_BRICK_WALL.get(), BASALT_BRICK_BLOCK.get());
        wallItemBlock(RUBY_BLOCK_WALL.get(), RUBY_BLOCK.get());
        wallItemBlock(SAPPHIRE_BLOCK_WALL.get(), SAPPHIRE_BLOCK.get());
        wallItemBlock(PERIDOT_BLOCK_WALL.get(), PERIDOT_BLOCK.get());
        wallItemBlock(ELECTROTINE_BLOCK_WALL.get(), ELECTROTINE_BLOCK.get());

        /* Items */

        // Ingots / dusts / gems
        generated(RAW_TIN_ITEM);
        generated(TIN_INGOT_ITEM);
        generated(RAW_SILVER_ITEM);
        generated(SILVER_INGOT_ITEM);

        /* Tools */

        generated(WOOL_GIN);
        handheld(ATHAME);

        handheld(RUBY_AXE);
        handheld(SAPPHIRE_AXE);
        handheld(PERIDOT_AXE);

        handheld(RUBY_PICKAXE);
        handheld(SAPPHIRE_PICKAXE);
        handheld(PERIDOT_PICKAXE);

        handheld(RUBY_SHOVEL);
        handheld(SAPPHIRE_SHOVEL);
        handheld(PERIDOT_SHOVEL);

        handheld(RUBY_HOE);
        handheld(SAPPHIRE_HOE);
        handheld(PERIDOT_HOE);

        handheld(RUBY_SWORD);
        handheld(SAPPHIRE_SWORD);
        handheld(PERIDOT_SWORD);

        handheld(GOLD_SAW);
        handheld(RUBY_SAW);
        handheld(SAPPHIRE_SAW);
        handheld(PERIDOT_SAW);

        handheld(WOOD_SICKLE);
        handheld(STONE_SICKLE);
        handheld(GOLD_SICKLE);
        handheld(IRON_SICKLE);
        handheld(DIAMOND_SICKLE);
        handheld(RUBY_SICKLE);
        handheld(SAPPHIRE_SICKLE);
        handheld(PERIDOT_SICKLE);

        /* Armor */
        generated(RUBY_HELMET);
        generated(SAPPHIRE_HELMET);
        generated(PERIDOT_HELMET);

        generated(RUBY_CHESTPLATE);
        generated(SAPPHIRE_CHESTPLATE);
        generated(PERIDOT_CHESTPLATE);

        generated(RUBY_LEGGINGS);
        generated(SAPPHIRE_LEGGINGS);
        generated(PERIDOT_LEGGINGS);

        generated(RUBY_BOOTS);
        generated(SAPPHIRE_BOOTS);
        generated(PERIDOT_BOOTS);

        /* Backpacks */
        generated(WHITE_BACKPACK);
        generated(ORANGE_BACKPACK);
        generated(MAGENTA_BACKPACK);
        generated(LIGHT_BLUE_BACKPACK);
        generated(YELLOW_BACKPACK);
        generated(LIME_BACKPACK);
        generated(PINK_BACKPACK);
        generated(GRAY_BACKPACK);
        generated(LIGHT_GRAY_BACKPACK);
        generated(CYAN_BACKPACK);
        generated(PURPLE_BACKPACK);
        generated(BLUE_BACKPACK);
        generated(BROWN_BACKPACK);
        generated(GREEN_BACKPACK);
        generated(RED_BACKPACK);
        generated(BLACK_BACKPACK);
    }

    private void wallItemBlock(Block wallBlock, Block textureBlock) {
        getSimple(wallBlock)
                .noTexture()
                .parent(wallInventory(wallBlock.getRegistryName() + "_inventory", blockTexture(textureBlock)));
    }
}
