package mrtjp.projectred.exploration.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;

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
        simpleItemBlock(RUBY_ORE_BLOCK);
        simpleItemBlock(SAPPHIRE_ORE_BLOCK);
        simpleItemBlock(PERIDOT_ORE_BLOCK);
        simpleItemBlock(COPPER_ORE_BLOCK);
        simpleItemBlock(TIN_ORE_BLOCK);
        simpleItemBlock(SILVER_ORE_BLOCK);
        simpleItemBlock(ELECTROTINE_ORE_BLOCK);

        // Decorative Blocks
        simpleItemBlock(MARBLE_BLOCK);
        simpleItemBlock(MARBLE_BRICK_BLOCK);
        simpleItemBlock(BASALT_BLOCK);
        simpleItemBlock(BASALT_COBBLE_BLOCK);
        simpleItemBlock(BASALT_BRICK_BLOCK);
        simpleItemBlock(RUBY_BLOCK);
        simpleItemBlock(SAPPHIRE_BLOCK);
        simpleItemBlock(PERIDOT_BLOCK);
        simpleItemBlock(COPPER_BLOCK);
        simpleItemBlock(TIN_BLOCK);
        simpleItemBlock(SILVER_BLOCK);
        simpleItemBlock(ELECTROTINE_BLOCK);

        // Walls
        wallItemBlock(MARBLE_WALL, MARBLE_BLOCK);
        wallItemBlock(MARBLE_BRICK_WALL, MARBLE_BRICK_BLOCK);
        wallItemBlock(BASALT_WALL, BASALT_BLOCK);
        wallItemBlock(BASALT_COBBLE_WALL, BASALT_COBBLE_BLOCK);
        wallItemBlock(BASALT_BRICK_WALL, BASALT_BRICK_BLOCK);
        wallItemBlock(RUBY_BLOCK_WALL, RUBY_BLOCK);
        wallItemBlock(SAPPHIRE_BLOCK_WALL, SAPPHIRE_BLOCK);
        wallItemBlock(PERIDOT_BLOCK_WALL, PERIDOT_BLOCK);
        wallItemBlock(COPPER_BLOCK_WALL, COPPER_BLOCK);
        wallItemBlock(TIN_BLOCK_WALL, TIN_BLOCK);
        wallItemBlock(SILVER_BLOCK_WALL, SILVER_BLOCK);
        wallItemBlock(ELECTROTINE_BLOCK_WALL, ELECTROTINE_BLOCK);

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
