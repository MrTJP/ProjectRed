package mrtjp.projectred.exploration.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.BlockTagsProvider;
import net.minecraft.tags.BlockTags;
import net.minecraftforge.common.Tags;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nullable;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;
import static mrtjp.projectred.exploration.init.ExplorationTags.*;

public class ExplorationBlockTagsProvider extends BlockTagsProvider {

    public ExplorationBlockTagsProvider(DataGenerator gen, @Nullable ExistingFileHelper existingFileHelper) {
        super(gen, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Exploration Block Tags";
    }

    @Override
    protected void addTags() {
        tag(BlockTags.WALLS).add(
                MARBLE_WALL,
                MARBLE_BRICK_WALL,
                BASALT_WALL,
                BASALT_COBBLE_WALL,
                BASALT_BRICK_WALL,
                RUBY_BLOCK_WALL,
                SAPPHIRE_BLOCK_WALL,
                PERIDOT_BLOCK_WALL,
                ELECTROTINE_BLOCK_WALL);

        tag(Tags.Blocks.ORES)
                .addTag(RUBY_ORES_BLOCK_TAG)
                .addTag(SAPPHIRE_ORES_BLOCK_TAG)
                .addTag(PERIDOT_ORES_BLOCK_TAG)
                .addTag(COPPER_ORES_BLOCK_TAG)
                .addTag(TIN_ORES_BLOCK_TAG)
                .addTag(SILVER_ORES_BLOCK_TAG)
                .addTag(ELECTROTINE_ORES_BLOCK_TAG);

        tag(Tags.Blocks.STORAGE_BLOCKS)
                .addTag(RUBY_STORAGE_BLOCK_TAG)
                .addTag(SAPPHIRE_STORAGE_BLOCK_TAG)
                .addTag(PERIDOT_STORAGE_BLOCK_TAG)
                .addTag(COPPER_STORAGE_BLOCK_TAG)
                .addTag(ELECTROTINE_STORAGE_BLOCK_TAG)
                .addTag(RAW_TIN_STORAGE_BLOCK_TAG)
                .addTag(TIN_STORAGE_BLOCK_TAG)
                .addTag(RAW_SILVER_STORAGE_BLOCK_TAG)
                .addTag(SILVER_STORAGE_BLOCK_TAG);

        /* Attach blocks to tags */

        tag(BlockTags.MINEABLE_WITH_PICKAXE)
                .add(RUBY_ORE_BLOCK)
                .add(DEEPSLATE_RUBY_ORE_BLOCK)
                .add(SAPPHIRE_ORE_BLOCK)
                .add(DEEPSLATE_SAPPHIRE_ORE_BLOCK)
                .add(PERIDOT_ORE_BLOCK)
                .add(DEEPSLATE_PERIDOT_ORE_BLOCK)
                .add(ELECTROTINE_ORE_BLOCK)
                .add(DEEPSLATE_ELECTROTINE_ORE_BLOCK)
                .add(TIN_ORE_BLOCK)
                .add(DEEPSLATE_TIN_ORE_BLOCK)
                .add(SILVER_ORE_BLOCK)
                .add(DEEPSLATE_SILVER_ORE_BLOCK)
                .add(MARBLE_BLOCK)
                .add(MARBLE_BRICK_BLOCK)
                .add(BASALT_BLOCK)
                .add(BASALT_COBBLE_BLOCK)
                .add(RUBY_BLOCK)
                .add(SAPPHIRE_BLOCK)
                .add(PERIDOT_BLOCK)
                .add(ELECTROTINE_BLOCK)
                .add(RAW_TIN_BLOCK)
                .add(TIN_BLOCK)
                .add(RAW_SILVER_BLOCK)
                .add(SILVER_BLOCK)
                .add(MARBLE_WALL)
                .add(MARBLE_BRICK_WALL)
                .add(BASALT_WALL)
                .add(BASALT_COBBLE_WALL)
                .add(BASALT_BRICK_WALL)
                .add(RUBY_BLOCK_WALL)
                .add(SAPPHIRE_BLOCK_WALL)
                .add(PERIDOT_BLOCK_WALL)
                .add(ELECTROTINE_BLOCK_WALL);

        tag(BlockTags.NEEDS_STONE_TOOL)
                // Ores
                .add(TIN_ORE_BLOCK)
                .add(DEEPSLATE_TIN_ORE_BLOCK)
                // Decorative blocks
                .add(MARBLE_BLOCK)
                .add(MARBLE_BRICK_BLOCK)
                .add(BASALT_BLOCK)
                .add(BASALT_COBBLE_BLOCK)
                .add(BASALT_BRICK_BLOCK)
                .add(ELECTROTINE_BLOCK)
                .add(RAW_TIN_BLOCK)
                .add(TIN_BLOCK)
                // Walls
                .add(MARBLE_WALL)
                .add(MARBLE_BRICK_WALL)
                .add(BASALT_WALL)
                .add(BASALT_COBBLE_WALL)
                .add(BASALT_BRICK_WALL)
                .add(ELECTROTINE_BLOCK_WALL);

        tag(BlockTags.NEEDS_IRON_TOOL)
                // Ores
                .add(RUBY_ORE_BLOCK)
                .add(DEEPSLATE_RUBY_ORE_BLOCK)
                .add(SAPPHIRE_ORE_BLOCK)
                .add(DEEPSLATE_SAPPHIRE_ORE_BLOCK)
                .add(PERIDOT_ORE_BLOCK)
                .add(DEEPSLATE_PERIDOT_ORE_BLOCK)
                .add(ELECTROTINE_ORE_BLOCK)
                .add(DEEPSLATE_ELECTROTINE_ORE_BLOCK)
                .add(SILVER_ORE_BLOCK)
                .add(DEEPSLATE_SILVER_ORE_BLOCK)
                // Decorative blocks
                .add(RUBY_BLOCK)
                .add(SAPPHIRE_BLOCK)
                .add(PERIDOT_BLOCK)
                .add(RAW_SILVER_BLOCK)
                .add(SILVER_BLOCK)
                // Walls
                .add(RUBY_BLOCK_WALL)
                .add(SAPPHIRE_BLOCK_WALL)
                .add(PERIDOT_BLOCK_WALL);

        tag(RUBY_ORES_BLOCK_TAG).add(RUBY_ORE_BLOCK).add(DEEPSLATE_RUBY_ORE_BLOCK);
        tag(SAPPHIRE_ORES_BLOCK_TAG).add(SAPPHIRE_ORE_BLOCK).add(DEEPSLATE_SAPPHIRE_ORE_BLOCK);
        tag(PERIDOT_ORES_BLOCK_TAG).add(PERIDOT_ORE_BLOCK).add(DEEPSLATE_PERIDOT_ORE_BLOCK);
        tag(TIN_ORES_BLOCK_TAG).add(TIN_ORE_BLOCK).add(DEEPSLATE_TIN_ORE_BLOCK);
        tag(SILVER_ORES_BLOCK_TAG).add(SILVER_ORE_BLOCK).add(DEEPSLATE_SILVER_ORE_BLOCK);
        tag(ELECTROTINE_ORES_BLOCK_TAG).add(ELECTROTINE_ORE_BLOCK).add(DEEPSLATE_ELECTROTINE_ORE_BLOCK);

        tag(Tags.Blocks.ORES_IN_GROUND_STONE)
                .add(RUBY_ORE_BLOCK)
                .add(SAPPHIRE_ORE_BLOCK)
                .add(PERIDOT_ORE_BLOCK)
                .add(TIN_ORE_BLOCK)
                .add(SILVER_ORE_BLOCK)
                .add(ELECTROTINE_ORE_BLOCK);

        tag(Tags.Blocks.ORES_IN_GROUND_DEEPSLATE)
                .add(DEEPSLATE_RUBY_ORE_BLOCK)
                .add(DEEPSLATE_SAPPHIRE_ORE_BLOCK)
                .add(DEEPSLATE_PERIDOT_ORE_BLOCK)
                .add(DEEPSLATE_TIN_ORE_BLOCK)
                .add(DEEPSLATE_SILVER_ORE_BLOCK)
                .add(DEEPSLATE_ELECTROTINE_ORE_BLOCK);

        tag(MARBLE_BLOCK_TAG).add(MARBLE_BLOCK);
        tag(BASALT_BLOCK_TAG).add(BASALT_BLOCK);
        tag(RUBY_STORAGE_BLOCK_TAG).add(RUBY_BLOCK);
        tag(SAPPHIRE_STORAGE_BLOCK_TAG).add(SAPPHIRE_BLOCK);
        tag(PERIDOT_STORAGE_BLOCK_TAG).add(PERIDOT_BLOCK);
        tag(ELECTROTINE_STORAGE_BLOCK_TAG).add(ELECTROTINE_BLOCK);
        tag(RAW_TIN_STORAGE_BLOCK_TAG).add(RAW_TIN_BLOCK);
        tag(TIN_STORAGE_BLOCK_TAG).add(TIN_BLOCK);
        tag(RAW_SILVER_STORAGE_BLOCK_TAG).add(RAW_SILVER_BLOCK);
        tag(SILVER_STORAGE_BLOCK_TAG).add(SILVER_BLOCK);
    }
}
