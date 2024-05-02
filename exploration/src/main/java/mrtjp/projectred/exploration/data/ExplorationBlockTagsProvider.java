package mrtjp.projectred.exploration.data;

import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.tags.BlockTags;
import net.minecraftforge.common.Tags;
import net.minecraftforge.common.data.BlockTagsProvider;
import net.minecraftforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationTags.*;

public class ExplorationBlockTagsProvider extends BlockTagsProvider {

    public ExplorationBlockTagsProvider(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider, @Nullable ExistingFileHelper existingFileHelper) {
        super(output, lookupProvider, MOD_ID, existingFileHelper);
    }

    @Override
    protected void addTags(HolderLookup.Provider lookup) {
        tag(BlockTags.WALLS).add(
                MARBLE_WALL.get(),
                MARBLE_BRICK_WALL.get(),
                BASALT_WALL.get(),
                BASALT_COBBLE_WALL.get(),
                BASALT_BRICK_WALL.get(),
                RUBY_BLOCK_WALL.get(),
                SAPPHIRE_BLOCK_WALL.get(),
                PERIDOT_BLOCK_WALL.get(),
                ELECTROTINE_BLOCK_WALL.get());

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
                .add(RUBY_ORE_BLOCK.get())
                .add(DEEPSLATE_RUBY_ORE_BLOCK.get())
                .add(SAPPHIRE_ORE_BLOCK.get())
                .add(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get())
                .add(PERIDOT_ORE_BLOCK.get())
                .add(DEEPSLATE_PERIDOT_ORE_BLOCK.get())
                .add(ELECTROTINE_ORE_BLOCK.get())
                .add(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get())
                .add(TIN_ORE_BLOCK.get())
                .add(DEEPSLATE_TIN_ORE_BLOCK.get())
                .add(SILVER_ORE_BLOCK.get())
                .add(DEEPSLATE_SILVER_ORE_BLOCK.get())
                .add(MARBLE_BLOCK.get())
                .add(MARBLE_BRICK_BLOCK.get())
                .add(BASALT_BLOCK.get())
                .add(BASALT_COBBLE_BLOCK.get())
                .add(RUBY_BLOCK.get())
                .add(SAPPHIRE_BLOCK.get())
                .add(PERIDOT_BLOCK.get())
                .add(ELECTROTINE_BLOCK.get())
                .add(RAW_TIN_BLOCK.get())
                .add(TIN_BLOCK.get())
                .add(RAW_SILVER_BLOCK.get())
                .add(SILVER_BLOCK.get())
                .add(MARBLE_WALL.get())
                .add(MARBLE_BRICK_WALL.get())
                .add(BASALT_WALL.get())
                .add(BASALT_COBBLE_WALL.get())
                .add(BASALT_BRICK_WALL.get())
                .add(RUBY_BLOCK_WALL.get())
                .add(SAPPHIRE_BLOCK_WALL.get())
                .add(PERIDOT_BLOCK_WALL.get())
                .add(ELECTROTINE_BLOCK_WALL.get());

        tag(BlockTags.NEEDS_STONE_TOOL)
                // Ores
                .add(TIN_ORE_BLOCK.get())
                .add(DEEPSLATE_TIN_ORE_BLOCK.get())
                // Decorative blocks
                .add(MARBLE_BLOCK.get())
                .add(MARBLE_BRICK_BLOCK.get())
                .add(BASALT_BLOCK.get())
                .add(BASALT_COBBLE_BLOCK.get())
                .add(BASALT_BRICK_BLOCK.get())
                .add(ELECTROTINE_BLOCK.get())
                .add(RAW_TIN_BLOCK.get())
                .add(TIN_BLOCK.get())
                // Walls
                .add(MARBLE_WALL.get())
                .add(MARBLE_BRICK_WALL.get())
                .add(BASALT_WALL.get())
                .add(BASALT_COBBLE_WALL.get())
                .add(BASALT_BRICK_WALL.get())
                .add(ELECTROTINE_BLOCK_WALL.get());

        tag(BlockTags.NEEDS_IRON_TOOL)
                // Ores
                .add(RUBY_ORE_BLOCK.get())
                .add(DEEPSLATE_RUBY_ORE_BLOCK.get())
                .add(SAPPHIRE_ORE_BLOCK.get())
                .add(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get())
                .add(PERIDOT_ORE_BLOCK.get())
                .add(DEEPSLATE_PERIDOT_ORE_BLOCK.get())
                .add(ELECTROTINE_ORE_BLOCK.get())
                .add(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get())
                .add(SILVER_ORE_BLOCK.get())
                .add(DEEPSLATE_SILVER_ORE_BLOCK.get())
                // Decorative blocks
                .add(RUBY_BLOCK.get())
                .add(SAPPHIRE_BLOCK.get())
                .add(PERIDOT_BLOCK.get())
                .add(RAW_SILVER_BLOCK.get())
                .add(SILVER_BLOCK.get())
                // Walls
                .add(RUBY_BLOCK_WALL.get())
                .add(SAPPHIRE_BLOCK_WALL.get())
                .add(PERIDOT_BLOCK_WALL.get());

        tag(RUBY_ORES_BLOCK_TAG).add(RUBY_ORE_BLOCK.get()).add(DEEPSLATE_RUBY_ORE_BLOCK.get());
        tag(SAPPHIRE_ORES_BLOCK_TAG).add(SAPPHIRE_ORE_BLOCK.get()).add(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get());
        tag(PERIDOT_ORES_BLOCK_TAG).add(PERIDOT_ORE_BLOCK.get()).add(DEEPSLATE_PERIDOT_ORE_BLOCK.get());
        tag(TIN_ORES_BLOCK_TAG).add(TIN_ORE_BLOCK.get()).add(DEEPSLATE_TIN_ORE_BLOCK.get());
        tag(SILVER_ORES_BLOCK_TAG).add(SILVER_ORE_BLOCK.get()).add(DEEPSLATE_SILVER_ORE_BLOCK.get());
        tag(ELECTROTINE_ORES_BLOCK_TAG).add(ELECTROTINE_ORE_BLOCK.get()).add(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get());

        tag(Tags.Blocks.ORES_IN_GROUND_STONE)
                .add(RUBY_ORE_BLOCK.get())
                .add(SAPPHIRE_ORE_BLOCK.get())
                .add(PERIDOT_ORE_BLOCK.get())
                .add(TIN_ORE_BLOCK.get())
                .add(SILVER_ORE_BLOCK.get())
                .add(ELECTROTINE_ORE_BLOCK.get());

        tag(Tags.Blocks.ORES_IN_GROUND_DEEPSLATE)
                .add(DEEPSLATE_RUBY_ORE_BLOCK.get())
                .add(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get())
                .add(DEEPSLATE_PERIDOT_ORE_BLOCK.get())
                .add(DEEPSLATE_TIN_ORE_BLOCK.get())
                .add(DEEPSLATE_SILVER_ORE_BLOCK.get())
                .add(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get());

        tag(MARBLE_BLOCK_TAG).add(MARBLE_BLOCK.get());
        tag(BASALT_BLOCK_TAG).add(BASALT_BLOCK.get());
        tag(RUBY_STORAGE_BLOCK_TAG).add(RUBY_BLOCK.get());
        tag(SAPPHIRE_STORAGE_BLOCK_TAG).add(SAPPHIRE_BLOCK.get());
        tag(PERIDOT_STORAGE_BLOCK_TAG).add(PERIDOT_BLOCK.get());
        tag(ELECTROTINE_STORAGE_BLOCK_TAG).add(ELECTROTINE_BLOCK.get());
        tag(RAW_TIN_STORAGE_BLOCK_TAG).add(RAW_TIN_BLOCK.get());
        tag(TIN_STORAGE_BLOCK_TAG).add(TIN_BLOCK.get());
        tag(RAW_SILVER_STORAGE_BLOCK_TAG).add(RAW_SILVER_BLOCK.get());
        tag(SILVER_STORAGE_BLOCK_TAG).add(SILVER_BLOCK.get());
    }
}
