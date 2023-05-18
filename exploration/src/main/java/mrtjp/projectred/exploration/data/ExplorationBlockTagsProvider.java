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
                COPPER_BLOCK_WALL,
                TIN_BLOCK_WALL,
                SILVER_BLOCK_WALL,
                ELECTROTINE_BLOCK_WALL);

        tag(Tags.Blocks.ORES)
                .addTag(RUBY_ORE_BLOCK_TAG)
                .addTag(SAPPHIRE_ORE_BLOCK_TAG)
                .addTag(PERIDOT_ORE_BLOCK_TAG)
                .addTag(COPPER_ORE_BLOCK_TAG)
                .addTag(TIN_ORE_BLOCK_TAG)
                .addTag(SILVER_ORE_BLOCK_TAG)
                .addTag(ELECTROTINE_ORE_BLOCK_TAG);

        tag(Tags.Blocks.STORAGE_BLOCKS)
                .addTag(RUBY_STORAGE_BLOCK_TAG)
                .addTag(SAPPHIRE_STORAGE_BLOCK_TAG)
                .addTag(PERIDOT_STORAGE_BLOCK_TAG)
                .addTag(COPPER_STORAGE_BLOCK_TAG)
                .addTag(TIN_STORAGE_BLOCK_TAG)
                .addTag(SILVER_STORAGE_BLOCK_TAG)
                .addTag(ELECTROTINE_STORAGE_BLOCK_TAG);

        /* Attach blocks to tags */

        tag(BlockTags.MINEABLE_WITH_PICKAXE)
                .add(RUBY_ORE_BLOCK)
                .add(SAPPHIRE_ORE_BLOCK)
                .add(PERIDOT_ORE_BLOCK)
                .add(COPPER_ORE_BLOCK)
                .add(TIN_ORE_BLOCK)
                .add(SILVER_ORE_BLOCK)
                .add(ELECTROTINE_ORE_BLOCK)
                .add(MARBLE_BLOCK)
                .add(MARBLE_BRICK_BLOCK)
                .add(BASALT_BLOCK)
                .add(BASALT_COBBLE_BLOCK)
                .add(RUBY_BLOCK)
                .add(SAPPHIRE_BLOCK)
                .add(PERIDOT_BLOCK)
                .add(COPPER_BLOCK)
                .add(TIN_BLOCK)
                .add(SILVER_BLOCK)
                .add(ELECTROTINE_BLOCK)
                .add(MARBLE_WALL)
                .add(MARBLE_BRICK_WALL)
                .add(BASALT_WALL)
                .add(BASALT_COBBLE_WALL)
                .add(BASALT_BRICK_WALL)
                .add(RUBY_BLOCK_WALL)
                .add(SAPPHIRE_BLOCK_WALL)
                .add(PERIDOT_BLOCK_WALL)
                .add(COPPER_BLOCK_WALL)
                .add(TIN_BLOCK_WALL)
                .add(SILVER_BLOCK_WALL)
                .add(ELECTROTINE_BLOCK_WALL);

        tag(BlockTags.NEEDS_STONE_TOOL)
                .add(COPPER_ORE_BLOCK)
                .add(TIN_ORE_BLOCK)
                .add(MARBLE_BLOCK)
                .add(MARBLE_BRICK_BLOCK)
                .add(BASALT_BLOCK)
                .add(BASALT_COBBLE_BLOCK)
                .add(BASALT_BRICK_BLOCK)
                .add(COPPER_BLOCK)
                .add(TIN_BLOCK)
                .add(MARBLE_WALL)
                .add(MARBLE_BRICK_WALL)
                .add(BASALT_WALL)
                .add(BASALT_COBBLE_WALL)
                .add(BASALT_BRICK_WALL)
                .add(COPPER_BLOCK_WALL)
                .add(TIN_BLOCK_WALL);

        tag(BlockTags.NEEDS_IRON_TOOL)
                .add(RUBY_ORE_BLOCK)
                .add(SAPPHIRE_ORE_BLOCK)
                .add(PERIDOT_ORE_BLOCK)
                .add(SILVER_ORE_BLOCK)
                .add(ELECTROTINE_ORE_BLOCK)
                .add(RUBY_BLOCK)
                .add(SAPPHIRE_BLOCK)
                .add(PERIDOT_BLOCK)
                .add(SILVER_BLOCK)
                .add(ELECTROTINE_BLOCK)
                .add(RUBY_BLOCK_WALL)
                .add(SAPPHIRE_BLOCK_WALL)
                .add(PERIDOT_BLOCK_WALL)
                .add(SILVER_BLOCK_WALL)
                .add(ELECTROTINE_BLOCK_WALL);

        tag(RUBY_ORE_BLOCK_TAG).add(RUBY_ORE_BLOCK);
        tag(SAPPHIRE_ORE_BLOCK_TAG).add(SAPPHIRE_ORE_BLOCK);
        tag(PERIDOT_ORE_BLOCK_TAG).add(PERIDOT_ORE_BLOCK);
        tag(COPPER_ORE_BLOCK_TAG).add(COPPER_ORE_BLOCK);
        tag(TIN_ORE_BLOCK_TAG).add(TIN_ORE_BLOCK);
        tag(SILVER_ORE_BLOCK_TAG).add(SILVER_ORE_BLOCK);
        tag(ELECTROTINE_ORE_BLOCK_TAG).add(ELECTROTINE_ORE_BLOCK);

        tag(MARBLE_BLOCK_TAG).add(MARBLE_BLOCK);
        tag(BASALT_BLOCK_TAG).add(BASALT_BLOCK);
        tag(RUBY_STORAGE_BLOCK_TAG).add(RUBY_BLOCK);
        tag(SAPPHIRE_STORAGE_BLOCK_TAG).add(SAPPHIRE_BLOCK);
        tag(PERIDOT_STORAGE_BLOCK_TAG).add(PERIDOT_BLOCK);
        tag(COPPER_STORAGE_BLOCK_TAG).add(COPPER_BLOCK);
        tag(TIN_STORAGE_BLOCK_TAG).add(TIN_BLOCK);
        tag(SILVER_STORAGE_BLOCK_TAG).add(SILVER_BLOCK);
        tag(ELECTROTINE_STORAGE_BLOCK_TAG).add(ELECTROTINE_BLOCK);
    }
}
