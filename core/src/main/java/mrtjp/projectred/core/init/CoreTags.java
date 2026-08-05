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
    public static final TagKey<Item> NICKEL_INGOT_TAG = tag("ingots/nickel");
    public static final TagKey<Item> ALUMINUM_INGOT_TAG = tag("ingots/aluminum");
    public static final TagKey<Item> BRONZE_INGOT_TAG = tag("ingots/bronze");
    public static final TagKey<Item> COPPER_INGOT_TAG = tag("c:/ingots/copper");
    public static final TagKey<Item> PLATINUM_INGOT_TAG = tag("c:/ingots/platinum");
    public static final TagKey<Item> LEAD_INGOT_TAG = tag("c:/ingots/lead");
    public static final TagKey<Item> INVAR_INGOT_TAG = tag("c:/ingots/invar");
    public static final TagKey<Item> ELECTRUM_INGOT_TAG = tag("c:/ingots/electrum");
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
