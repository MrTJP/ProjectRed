package mrtjp.projectred.transmission.init;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;

public class TransmissionTags {

    public static final TagKey<Item> INSULATED_WIRE_ITEM_TAG = ItemTags.create(new ResourceLocation(MOD_ID, "insulated_wire"));
    public static final TagKey<Item> BUNDLED_WIRE_ITEM_TAG = ItemTags.create(new ResourceLocation(MOD_ID, "bundled_wire"));
    public static final TagKey<Item> FRAMED_INSULATED_WIRE_ITEM_TAG = ItemTags.create(new ResourceLocation(MOD_ID, "framed_insulated_wire"));
    public static final TagKey<Item> FRAMED_BUNDLED_WIRE_ITEM_TAG = ItemTags.create(new ResourceLocation(MOD_ID, "framed_bundled_wire"));

}
