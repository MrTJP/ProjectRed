package mrtjp.projectred.core.init;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class CoreTags {

    /* Block Tags */

    /* Item Tags */
    public static final TagKey<Item> COPPER_INGOT_TAG = ItemTags.create(new ResourceLocation("forge:ingots/copper"));
    public static final TagKey<Item> TIN_INGOT_TAG = ItemTags.create(new ResourceLocation("forge:ingots/tin"));
    public static final TagKey<Item> SILVER_INGOT_TAG = ItemTags.create(new ResourceLocation("forge:ingots/silver"));
    public static final TagKey<Item> RED_ALLOY_INGOT_TAG = ItemTags.create(new ResourceLocation("forge:ingots/red_alloy"));
    public static final TagKey<Item> ELECTROTINE_ALLOY_INGOT_TAG = ItemTags.create(new ResourceLocation("forge:ingots/electrotine_alloy"));

    public static final TagKey<Item> RUBY_GEM_TAG = ItemTags.create(new ResourceLocation("forge:gems/ruby"));
    public static final TagKey<Item> SAPPHIRE_GEM_TAG = ItemTags.create(new ResourceLocation("forge:gems/sapphire"));
    public static final TagKey<Item> PERIDOT_GEM_TAG = ItemTags.create(new ResourceLocation("forge:gems/peridot"));

    public static final TagKey<Item> ELECTROTINE_DUST_TAG = ItemTags.create(new ResourceLocation("forge:dusts/electrotine"));

    public static final TagKey<Item> ILLUMAR_TAG = ItemTags.create(new ResourceLocation(MOD_ID, "illumars"));
}
