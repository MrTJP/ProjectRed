package mrtjp.projectred.core.data;

import net.minecraft.data.BlockTagsProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.ItemTagsProvider;
import net.minecraftforge.common.Tags;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nullable;

import static mrtjp.projectred.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreReferences.*;
import static mrtjp.projectred.core.init.CoreTags.*;

public class CoreItemTagsProvider extends ItemTagsProvider {

    public CoreItemTagsProvider(DataGenerator gen, @Nullable ExistingFileHelper helper) {
        super(gen, new BlockTagsProvider(gen, MOD_ID, helper), MOD_ID, helper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Core Item Tags";
    }

    @Override
    protected void addTags() {

        tag(Tags.Items.INGOTS)
                .addTag(COPPER_INGOT_TAG)
                .addTag(TIN_INGOT_TAG)
                .addTag(SILVER_INGOT_TAG)
                .addTag(RED_ALLOY_INGOT_TAG)
                .addTag(ELECTROTINE_ALLOY_INGOT_TAG);

        tag(Tags.Items.GEMS)
                .addTag(RUBY_GEM_TAG)
                .addTag(SAPPHIRE_GEM_TAG)
                .addTag(PERIDOT_GEM_TAG);

        tag(Tags.Items.DUSTS)
                .addTag(ELECTROTINE_DUST_TAG);

        tag(COPPER_INGOT_TAG).add(COPPER_INGOT_ITEM);
        tag(TIN_INGOT_TAG).add(TIN_INGOT_ITEM);
        tag(SILVER_INGOT_TAG).add(SILVER_INGOT_ITEM);
        tag(RED_ALLOY_INGOT_TAG).add(RED_ALLOY_INGOT_ITEM);
        tag(ELECTROTINE_ALLOY_INGOT_TAG).add(ELECTROTINE_ALLOY_INGOT_ITEM);
        tag(RUBY_GEM_TAG).add(RUBY_ITEM);
        tag(SAPPHIRE_GEM_TAG).add(SAPPHIRE_ITEM);
        tag(PERIDOT_GEM_TAG).add(PERIDOT_ITEM);
        tag(ELECTROTINE_DUST_TAG).add(ELECTROTINE_DUST_ITEM);
        tag(ILLUMAR_TAG)
                .add(WHITE_ILLUMAR_ITEM)
                .add(ORANGE_ILLUMAR_ITEM)
                .add(MAGENTA_ILLUMAR_ITEM)
                .add(LIGHT_BLUE_ILLUMAR_ITEM)
                .add(YELLOW_ILLUMAR_ITEM)
                .add(LIME_ILLUMAR_ITEM)
                .add(PINK_ILLUMAR_ITEM)
                .add(GRAY_ILLUMAR_ITEM)
                .add(LIGHT_GRAY_ILLUMAR_ITEM)
                .add(CYAN_ILLUMAR_ITEM)
                .add(PURPLE_ILLUMAR_ITEM)
                .add(BLUE_ILLUMAR_ITEM)
                .add(BROWN_ILLUMAR_ITEM)
                .add(GREEN_ILLUMAR_ITEM)
                .add(RED_ILLUMAR_ITEM)
                .add(BLACK_ILLUMAR_ITEM);
    }
}
