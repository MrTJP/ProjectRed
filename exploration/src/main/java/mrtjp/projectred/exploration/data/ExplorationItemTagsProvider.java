package mrtjp.projectred.exploration.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.ItemTagsProvider;
import net.minecraftforge.common.Tags;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nullable;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;
import static mrtjp.projectred.exploration.init.ExplorationTags.*;

public class ExplorationItemTagsProvider extends ItemTagsProvider {

    public ExplorationItemTagsProvider(DataGenerator gen, @Nullable ExistingFileHelper existingFileHelper) {
        super(gen, new ExplorationBlockTagsProvider(gen, existingFileHelper), MOD_ID, existingFileHelper);
    }

    @Override
    protected void addTags() {

        /* Attach tags to other tags */

        tag(Tags.Items.ORES)
                .addTag(RUBY_ORE_BLOCK_ITEM_TAG)
                .addTag(SAPPHIRE_ORE_BLOCK_ITEM_TAG)
                .addTag(PERIDOT_ORE_BLOCK_ITEM_TAG)
                .addTag(COPPER_ORE_BLOCK_ITEM_TAG)
                .addTag(TIN_ORE_BLOCK_ITEM_TAG)
                .addTag(SILVER_ORE_BLOCK_ITEM_TAG)
                .addTag(ELECTROTINE_ORE_BLOCK_ITEM_TAG);

        tag(Tags.Items.STORAGE_BLOCKS)
                .addTag(RUBY_STORAGE_BLOCK_ITEM_TAG)
                .addTag(SAPPHIRE_STORAGE_BLOCK_ITEM_TAG)
                .addTag(PERIDOT_STORAGE_BLOCK_ITEM_TAG)
                .addTag(COPPER_STORAGE_BLOCK_ITEM_TAG)
                .addTag(TIN_STORAGE_BLOCK_ITEM_TAG)
                .addTag(SILVER_STORAGE_BLOCK_ITEM_TAG)
                .addTag(ELECTROTINE_STORAGE_BLOCK_ITEM_TAG);

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

        tag(RUBY_ORE_BLOCK_ITEM_TAG).add(RUBY_ORE_BLOCK.asItem());
        tag(SAPPHIRE_ORE_BLOCK_ITEM_TAG).add(SAPPHIRE_ORE_BLOCK.asItem());
        tag(PERIDOT_ORE_BLOCK_ITEM_TAG).add(PERIDOT_ORE_BLOCK.asItem());
        tag(COPPER_ORE_BLOCK_ITEM_TAG).add(COPPER_ORE_BLOCK.asItem());
        tag(TIN_ORE_BLOCK_ITEM_TAG).add(TIN_ORE_BLOCK.asItem());
        tag(SILVER_ORE_BLOCK_ITEM_TAG).add(SILVER_ORE_BLOCK.asItem());
        tag(ELECTROTINE_ORE_BLOCK_ITEM_TAG).add(ELECTROTINE_ORE_BLOCK.asItem());

        tag(MARBLE_BLOCK_ITEM_TAG).add(MARBLE_BLOCK.asItem());
        tag(BASALT_BLOCK_ITEM_TAG).add(BASALT_BLOCK.asItem());
        tag(RUBY_STORAGE_BLOCK_ITEM_TAG).add(RUBY_BLOCK.asItem());
        tag(SAPPHIRE_STORAGE_BLOCK_ITEM_TAG).add(SAPPHIRE_BLOCK.asItem());
        tag(PERIDOT_STORAGE_BLOCK_ITEM_TAG).add(PERIDOT_BLOCK.asItem());
        tag(COPPER_STORAGE_BLOCK_ITEM_TAG).add(COPPER_BLOCK.asItem());
        tag(TIN_STORAGE_BLOCK_ITEM_TAG).add(TIN_BLOCK.asItem());
        tag(SILVER_STORAGE_BLOCK_ITEM_TAG).add(SILVER_BLOCK.asItem());
        tag(ELECTROTINE_STORAGE_BLOCK_ITEM_TAG).add(ELECTROTINE_BLOCK.asItem());

        tag(WHITE_BACKPACK_TAG).add(WHITE_BACKPACK);
        tag(ORANGE_BACKPACK_TAG).add(ORANGE_BACKPACK);
        tag(MAGENTA_BACKPACK_TAG).add(MAGENTA_BACKPACK);
        tag(LIGHT_BLUE_BACKPACK_TAG).add(LIGHT_BLUE_BACKPACK);
        tag(YELLOW_BACKPACK_TAG).add(YELLOW_BACKPACK);
        tag(LIME_BACKPACK_TAG).add(LIME_BACKPACK);
        tag(PINK_BACKPACK_TAG).add(PINK_BACKPACK);
        tag(GRAY_BACKPACK_TAG).add(GRAY_BACKPACK);
        tag(LIGHT_GRAY_BACKPACK_TAG).add(LIGHT_GRAY_BACKPACK);
        tag(CYAN_BACKPACK_TAG).add(CYAN_BACKPACK);
        tag(PURPLE_BACKPACK_TAG).add(PURPLE_BACKPACK);
        tag(BLUE_BACKPACK_TAG).add(BLUE_BACKPACK);
        tag(BROWN_BACKPACK_TAG).add(BROWN_BACKPACK);
        tag(GREEN_BACKPACK_TAG).add(GREEN_BACKPACK);
        tag(RED_BACKPACK_TAG).add(RED_BACKPACK);
        tag(BLACK_BACKPACK_TAG).add(BLACK_BACKPACK);
    }
}
