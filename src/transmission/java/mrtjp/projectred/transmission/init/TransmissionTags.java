package mrtjp.projectred.transmission.init;

import net.minecraft.item.Item;
import net.minecraft.tags.ITag;
import net.minecraft.tags.ItemTags;
import net.minecraft.util.ResourceLocation;

import static mrtjp.projectred.ProjectRedTransmission.MOD_ID;

public class TransmissionTags {

    public static final ITag.INamedTag<Item> INSULATED_WIRE_ITEM_TAG = ItemTags.bind(new ResourceLocation(MOD_ID, "insulated_wire").toString());
    public static final ITag.INamedTag<Item> BUNDLED_WIRE_ITEM_TAG = ItemTags.bind(new ResourceLocation(MOD_ID, "bundled_wire").toString());
    public static final ITag.INamedTag<Item> FRAMED_INSULATED_WIRE_ITEM_TAG = ItemTags.bind(new ResourceLocation(MOD_ID, "framed_insulated_wire").toString());
    public static final ITag.INamedTag<Item> FRAMED_BUNDLED_WIRE_ITEM_TAG = ItemTags.bind(new ResourceLocation(MOD_ID, "framed_bundled_wire").toString());

}
