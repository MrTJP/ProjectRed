package mrtjp.projectred.transmission.data;

import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.data.tags.ItemTagsProvider;
import net.minecraft.data.tags.TagsProvider;
import net.minecraft.world.level.block.Block;
import net.neoforged.neoforge.common.data.ExistingFileHelper;

import javax.annotation.Nullable;
import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;
import static mrtjp.projectred.transmission.init.TransmissionTags.*;

public class TransmissionItemTagsProvider extends ItemTagsProvider {

    public TransmissionItemTagsProvider(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider, CompletableFuture<TagsProvider.TagLookup<Block>> blockTags, @Nullable ExistingFileHelper helper) {
        super(output, lookupProvider, blockTags, MOD_ID, helper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Transmission Item Tags";
    }

    @Override
    protected void addTags(HolderLookup.Provider pProvider) {
        // Insulated wires
        var b = tag(INSULATED_WIRE_ITEM_TAG);
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
