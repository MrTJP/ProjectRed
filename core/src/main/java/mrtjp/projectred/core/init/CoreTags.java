package mrtjp.projectred.core.init;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class CoreTags {

    /* Block Tags */

    /* Item Tags */
    public static final TagKey<Item> TIN_INGOT_TAG = tag("ingots/tin");
    public static final TagKey<Item> SILVER_INGOT_TAG = tag("ingots/silver");
    public static final TagKey<Item> RED_ALLOY_INGOT_TAG = tag("ingots/red_alloy");
    public static final TagKey<Item> ELECTROTINE_ALLOY_INGOT_TAG = tag("ingots/electrotine_alloy");

    public static final TagKey<Item> RAW_TIN_TAG = tag("raw_materials/tin");
    public static final TagKey<Item> RAW_SILVER_TAG = tag("raw_materials/silver");

    public static final TagKey<Item> RUBY_GEM_TAG = tag("gems/ruby");
    public static final TagKey<Item> SAPPHIRE_GEM_TAG = tag("gems/sapphire");
    public static final TagKey<Item> PERIDOT_GEM_TAG = tag("gems/peridot");

    public static final TagKey<Item> ELECTROTINE_DUST_TAG = tag("dusts/electrotine");

    public static final TagKey<Item> ILLUMAR_TAG = prTag("illumars");

    private static TagKey<Item> tag(String path) {
        return ItemTags.create(ResourceLocation.fromNamespaceAndPath("c", path));
    }

    private static TagKey<Item> prTag(String path) {
        return ItemTags.create(ResourceLocation.fromNamespaceAndPath(MOD_ID, path));
    }
}
