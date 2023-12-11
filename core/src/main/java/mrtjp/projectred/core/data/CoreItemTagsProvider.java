package mrtjp.projectred.core.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.BlockTagsProvider;
import net.minecraft.data.tags.ItemTagsProvider;
import net.minecraftforge.common.Tags;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nullable;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreItems.*;
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
                .addTag(RED_ALLOY_INGOT_TAG)
                .addTag(ELECTROTINE_ALLOY_INGOT_TAG);

        tag(Tags.Items.GEMS)
                .addTag(RUBY_GEM_TAG)
                .addTag(SAPPHIRE_GEM_TAG)
                .addTag(PERIDOT_GEM_TAG);

        tag(Tags.Items.DUSTS)
                .addTag(ELECTROTINE_DUST_TAG);

        tag(RED_ALLOY_INGOT_TAG).add(RED_ALLOY_INGOT_ITEM.get());
        tag(ELECTROTINE_ALLOY_INGOT_TAG).add(ELECTROTINE_ALLOY_INGOT_ITEM.get());
        tag(RUBY_GEM_TAG).add(RUBY_ITEM.get());
        tag(SAPPHIRE_GEM_TAG).add(SAPPHIRE_ITEM.get());
        tag(PERIDOT_GEM_TAG).add(PERIDOT_ITEM.get());
        tag(ELECTROTINE_DUST_TAG).add(ELECTROTINE_DUST_ITEM.get());
        tag(ILLUMAR_TAG)
                .add(WHITE_ILLUMAR_ITEM.get())
                .add(ORANGE_ILLUMAR_ITEM.get())
                .add(MAGENTA_ILLUMAR_ITEM.get())
                .add(LIGHT_BLUE_ILLUMAR_ITEM.get())
                .add(YELLOW_ILLUMAR_ITEM.get())
                .add(LIME_ILLUMAR_ITEM.get())
                .add(PINK_ILLUMAR_ITEM.get())
                .add(GRAY_ILLUMAR_ITEM.get())
                .add(LIGHT_GRAY_ILLUMAR_ITEM.get())
                .add(CYAN_ILLUMAR_ITEM.get())
                .add(PURPLE_ILLUMAR_ITEM.get())
                .add(BLUE_ILLUMAR_ITEM.get())
                .add(BROWN_ILLUMAR_ITEM.get())
                .add(GREEN_ILLUMAR_ITEM.get())
                .add(RED_ILLUMAR_ITEM.get())
                .add(BLACK_ILLUMAR_ITEM.get());
    }
}
