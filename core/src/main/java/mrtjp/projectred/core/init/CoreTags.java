package mrtjp.projectred.core.init;

import net.minecraft.item.Item;
import net.minecraft.tags.ITag;
import net.minecraft.tags.ItemTags;
import net.minecraft.util.ResourceLocation;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class CoreTags {

    /* Block Tags */

    /* Item Tags */
    public static final ITag.INamedTag<Item> COPPER_INGOT_TAG = ItemTags.bind("forge:ingots/copper");
    public static final ITag.INamedTag<Item> TIN_INGOT_TAG = ItemTags.bind("forge:ingots/tin");
    public static final ITag.INamedTag<Item> SILVER_INGOT_TAG = ItemTags.bind("forge:ingots/silver");
    public static final ITag.INamedTag<Item> RED_ALLOY_INGOT_TAG = ItemTags.bind("forge:ingots/red_alloy");
    public static final ITag.INamedTag<Item> ELECTROTINE_ALLOY_INGOT_TAG = ItemTags.bind("forge:ingots/electrotine_alloy");

    public static final ITag.INamedTag<Item> RUBY_GEM_TAG = ItemTags.bind("forge:gems/ruby");
    public static final ITag.INamedTag<Item> SAPPHIRE_GEM_TAG = ItemTags.bind("forge:gems/sapphire");
    public static final ITag.INamedTag<Item> PERIDOT_GEM_TAG = ItemTags.bind("forge:gems/peridot");

    public static final ITag.INamedTag<Item> ELECTROTINE_DUST_TAG = ItemTags.bind("forge:dusts/electrotine");

    public static final ITag.INamedTag<Item> ILLUMAR_TAG = ItemTags.bind(new ResourceLocation(MOD_ID, "illumars").toString());
}
