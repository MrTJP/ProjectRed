package mrtjp.projectred.expansion.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.BlockTagsProvider;
import net.minecraft.tags.BlockTags;
import net.minecraftforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.Nullable;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

public class ExpansionBlockTagsProvider extends BlockTagsProvider {

    public ExpansionBlockTagsProvider(DataGenerator gen, @Nullable ExistingFileHelper existingFileHelper) {
        super(gen, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Block Tags";
    }

    @Override
    protected void addTags() {

        tag(BlockTags.MINEABLE_WITH_AXE)
                .add(BATTERY_BOX_BLOCK)
                .add(FRAME_BLOCK);

        tag(BlockTags.MINEABLE_WITH_PICKAXE)
                .add(PROJECT_BENCH_BLOCK)
                .add(CHARGING_BENCH_BLOCK)
                .add(AUTO_CRAFTER_BLOCK)
                .add(FIRE_STARTER_BLOCK)
                .add(FRAME_MOTOR_BLOCK)
                .add(FRAME_ACTUATOR_BLOCK);

        tag(BlockTags.NEEDS_STONE_TOOL)
                .add(PROJECT_BENCH_BLOCK)
                .add(CHARGING_BENCH_BLOCK)
                .add(AUTO_CRAFTER_BLOCK)
                .add(FIRE_STARTER_BLOCK)
                .add(FRAME_MOTOR_BLOCK)
                .add(FRAME_ACTUATOR_BLOCK);
    }
}
