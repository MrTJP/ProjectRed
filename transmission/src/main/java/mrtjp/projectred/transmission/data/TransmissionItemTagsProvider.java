package mrtjp.projectred.transmission.data;

import mrtjp.projectred.transmission.WireType;
import net.minecraft.data.BlockTagsProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.ItemTagsProvider;
import net.minecraft.data.TagsProvider;
import net.minecraft.item.Item;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nullable;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;
import static mrtjp.projectred.transmission.init.TransmissionTags.*;

public class TransmissionItemTagsProvider extends ItemTagsProvider {

    public TransmissionItemTagsProvider(DataGenerator generator, @Nullable ExistingFileHelper existingFileHelper) {
        super(generator, new BlockTagsProvider(generator, MOD_ID, existingFileHelper), MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Transmission Item Tags";
    }

    @Override
    protected void addTags() {
        // Insulated wires
        TagsProvider.Builder<Item> b = tag(INSULATED_WIRE_ITEM_TAG);
        for (WireType type : WireType.INSULATED_WIRES) {
            b.add(type.getItem());
        }

        // Bundled cables
        b = tag(BUNDLED_WIRE_ITEM_TAG);
        b.add(WireType.BUNDLED_NEUTRAL.getItem());
        for (WireType type : WireType.COLOURED_BUNDLED_WIRES) {
            b.add(type.getItem());
        }

        // Framed Insulated
        b = tag(FRAMED_INSULATED_WIRE_ITEM_TAG);
        for (WireType type : WireType.FRAMED_INSULATED_WIRES) {
            b.add(type.getItem());
        }

        // Framed Bundled
        b = tag(FRAMED_BUNDLED_WIRE_ITEM_TAG);
        b.add(WireType.FRAMED_BUNDLED_NEUTRAL.getItem());
        for (WireType type : WireType.FRAMED_COLOURED_BUNDLED_WIRES) {
            b.add(type.getItem());
        }
    }
}
