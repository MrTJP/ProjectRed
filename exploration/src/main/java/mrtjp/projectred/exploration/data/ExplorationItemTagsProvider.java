package mrtjp.projectred.exploration.data;

import codechicken.microblock.init.CBMicroblockTags;
import mrtjp.projectred.core.ProjectRedCore;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.data.tags.ItemTagsProvider;
import net.minecraft.data.tags.TagsProvider;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.common.Tags;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nullable;
import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.core.init.CoreTags.*;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationItems.*;
import static mrtjp.projectred.exploration.init.ExplorationTags.*;

public class ExplorationItemTagsProvider extends ItemTagsProvider {

    public ExplorationItemTagsProvider(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider, CompletableFuture<TagsProvider.TagLookup<Block>> blockTags, @Nullable ExistingFileHelper helper) {
        super(output, lookupProvider, blockTags, ProjectRedCore.MOD_ID, helper);
    }

    @Override
    protected void addTags(HolderLookup.Provider pProvider) {

        /* Attach tags to other tags */

        tag(Tags.Items.INGOTS)
                //Note: Defined in Core
                .addTag(TIN_INGOT_TAG)
                .addTag(SILVER_INGOT_TAG);

        tag(Tags.Items.RAW_MATERIALS)
                //Note: Defined in Core
                .addTag(RAW_TIN_TAG)
                .addTag(RAW_SILVER_TAG);

        tag(Tags.Items.ORES)
                .addTag(RUBY_ORES_BLOCK_ITEM_TAG)
                .addTag(SAPPHIRE_ORES_BLOCK_ITEM_TAG)
                .addTag(PERIDOT_ORES_BLOCK_ITEM_TAG)
                .addTag(COPPER_ORES_BLOCK_ITEM_TAG)
                .addTag(TIN_ORES_BLOCK_ITEM_TAG)
                .addTag(SILVER_ORES_BLOCK_ITEM_TAG)
                .addTag(ELECTROTINE_ORES_BLOCK_ITEM_TAG);

        tag(Tags.Items.STORAGE_BLOCKS)
                .addTag(RUBY_STORAGE_BLOCK_ITEM_TAG)
                .addTag(SAPPHIRE_STORAGE_BLOCK_ITEM_TAG)
                .addTag(PERIDOT_STORAGE_BLOCK_ITEM_TAG)
                .addTag(ELECTROTINE_STORAGE_BLOCK_ITEM_TAG)
                .addTag(RAW_TIN_STORAGE_BLOCK_ITEM_TAG)
                .addTag(TIN_STORAGE_BLOCK_ITEM_TAG)
                .addTag(RAW_SILVER_STORAGE_BLOCK_ITEM_TAG)
                .addTag(SILVER_STORAGE_BLOCK_ITEM_TAG);

        tag(BACKPACKS_TAG)
                .addTag(WHITE_BACKPACK_TAG)
                .addTag(ORANGE_BACKPACK_TAG)
                .addTag(MAGENTA_BACKPACK_TAG)
                .addTag(LIGHT_BLUE_BACKPACK_TAG)
                .addTag(YELLOW_BACKPACK_TAG)
                .addTag(LIME_BACKPACK_TAG)
                .addTag(PINK_BACKPACK_TAG)
                .addTag(GRAY_BACKPACK_TAG)
                .addTag(LIGHT_GRAY_BACKPACK_TAG)
                .addTag(CYAN_BACKPACK_TAG)
                .addTag(PURPLE_BACKPACK_TAG)
                .addTag(BLUE_BACKPACK_TAG)
                .addTag(BROWN_BACKPACK_TAG)
                .addTag(GREEN_BACKPACK_TAG)
                .addTag(RED_BACKPACK_TAG)
                .addTag(BLACK_BACKPACK_TAG);

        tag(BACKPACKS_DISALLOWED_TAG).addTag(BACKPACKS_TAG); // Items matching this tag wont be allowed in backpack inventory

        /* Attach items to tags */

        //Note: Defined in Core
        tag(TIN_INGOT_TAG).add(TIN_INGOT_ITEM.get());
        tag(SILVER_INGOT_TAG).add(SILVER_INGOT_ITEM.get());
        tag(RAW_TIN_TAG).add(RAW_TIN_ITEM.get());
        tag(RAW_SILVER_TAG).add(RAW_SILVER_ITEM.get());

        tag(RUBY_ORES_BLOCK_ITEM_TAG).add(RUBY_ORE_BLOCK.get().asItem()).add(DEEPSLATE_RUBY_ORE_BLOCK.get().asItem());
        tag(SAPPHIRE_ORES_BLOCK_ITEM_TAG).add(SAPPHIRE_ORE_BLOCK.get().asItem()).add(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get().asItem());
        tag(PERIDOT_ORES_BLOCK_ITEM_TAG).add(PERIDOT_ORE_BLOCK.get().asItem()).add(DEEPSLATE_PERIDOT_ORE_BLOCK.get().asItem());
        tag(TIN_ORES_BLOCK_ITEM_TAG).add(TIN_ORE_BLOCK.get().asItem()).add(DEEPSLATE_TIN_ORE_BLOCK.get().asItem());
        tag(SILVER_ORES_BLOCK_ITEM_TAG).add(SILVER_ORE_BLOCK.get().asItem()).add(DEEPSLATE_SILVER_ORE_BLOCK.get().asItem());
        tag(ELECTROTINE_ORES_BLOCK_ITEM_TAG).add(ELECTROTINE_ORE_BLOCK.get().asItem()).add(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get().asItem());

        tag(MARBLE_BLOCK_ITEM_TAG).add(MARBLE_BLOCK.get().asItem());
        tag(BASALT_BLOCK_ITEM_TAG).add(BASALT_BLOCK.get().asItem());
        tag(RUBY_STORAGE_BLOCK_ITEM_TAG).add(RUBY_BLOCK.get().asItem());
        tag(SAPPHIRE_STORAGE_BLOCK_ITEM_TAG).add(SAPPHIRE_BLOCK.get().asItem());
        tag(PERIDOT_STORAGE_BLOCK_ITEM_TAG).add(PERIDOT_BLOCK.get().asItem());
        tag(ELECTROTINE_STORAGE_BLOCK_ITEM_TAG).add(ELECTROTINE_BLOCK.get().asItem());
        tag(TIN_STORAGE_BLOCK_ITEM_TAG).add(TIN_BLOCK.get().asItem());
        tag(RAW_TIN_STORAGE_BLOCK_ITEM_TAG).add(RAW_TIN_BLOCK.get().asItem());
        tag(SILVER_STORAGE_BLOCK_ITEM_TAG).add(SILVER_BLOCK.get().asItem());
        tag(RAW_SILVER_STORAGE_BLOCK_ITEM_TAG).add(RAW_SILVER_BLOCK.get().asItem());

        tag(WHITE_BACKPACK_TAG).add(WHITE_BACKPACK.get());
        tag(ORANGE_BACKPACK_TAG).add(ORANGE_BACKPACK.get());
        tag(MAGENTA_BACKPACK_TAG).add(MAGENTA_BACKPACK.get());
        tag(LIGHT_BLUE_BACKPACK_TAG).add(LIGHT_BLUE_BACKPACK.get());
        tag(YELLOW_BACKPACK_TAG).add(YELLOW_BACKPACK.get());
        tag(LIME_BACKPACK_TAG).add(LIME_BACKPACK.get());
        tag(PINK_BACKPACK_TAG).add(PINK_BACKPACK.get());
        tag(GRAY_BACKPACK_TAG).add(GRAY_BACKPACK.get());
        tag(LIGHT_GRAY_BACKPACK_TAG).add(LIGHT_GRAY_BACKPACK.get());
        tag(CYAN_BACKPACK_TAG).add(CYAN_BACKPACK.get());
        tag(PURPLE_BACKPACK_TAG).add(PURPLE_BACKPACK.get());
        tag(BLUE_BACKPACK_TAG).add(BLUE_BACKPACK.get());
        tag(BROWN_BACKPACK_TAG).add(BROWN_BACKPACK.get());
        tag(GREEN_BACKPACK_TAG).add(GREEN_BACKPACK.get());
        tag(RED_BACKPACK_TAG).add(RED_BACKPACK.get());
        tag(BLACK_BACKPACK_TAG).add(BLACK_BACKPACK.get());

        tag(CBMicroblockTags.Items.TOOL_SAW)
                .add(GOLD_SAW.get())
                .add(RUBY_SAW.get())
                .add(SAPPHIRE_SAW.get())
                .add(PERIDOT_SAW.get());
    }
}
