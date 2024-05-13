package mrtjp.projectred.fabrication.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.BlockTagsProvider;
import net.minecraft.tags.BlockTags;
import net.minecraftforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.Nullable;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

public class FabricationBlockTagsProvider extends BlockTagsProvider {

    public FabricationBlockTagsProvider(DataGenerator gen, @Nullable ExistingFileHelper existingFileHelper) {
        super(gen, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Fabrication Block Tags";
    }

    @Override
    protected void addTags() {

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
