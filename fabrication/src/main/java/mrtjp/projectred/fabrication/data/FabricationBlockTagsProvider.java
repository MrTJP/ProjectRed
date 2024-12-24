package mrtjp.projectred.fabrication.data;

import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.tags.BlockTags;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

public class FabricationBlockTagsProvider extends BlockTagsProvider {

    public FabricationBlockTagsProvider(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider, @Nullable ExistingFileHelper existingFileHelper) {
        super(output, lookupProvider, MOD_ID, existingFileHelper);
    }

    @Override
    protected void addTags(HolderLookup.Provider lookup) {

        tag(BlockTags.MINEABLE_WITH_PICKAXE)
                .add(IC_WORKBENCH_BLOCK.get())
                .add(PLOTTING_TABLE_BLOCK.get())
                .add(LITHOGRAPHY_TABLE_BLOCK.get())
                .add(PACKAGING_TABLE_BLOCK.get());

        tag(BlockTags.NEEDS_STONE_TOOL)
                .add(IC_WORKBENCH_BLOCK.get())
                .add(PLOTTING_TABLE_BLOCK.get())
                .add(LITHOGRAPHY_TABLE_BLOCK.get())
                .add(PACKAGING_TABLE_BLOCK.get());
    }
}
