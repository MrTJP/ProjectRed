package mrtjp.projectred.expansion.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.model.generators.ItemModelBuilder;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;
import static mrtjp.projectred.expansion.init.ExpansionClientInit.ITEM_MODEL_PROPERTY_CHARGE_LEVEL;
import static mrtjp.projectred.expansion.init.ExpansionClientInit.ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN;
import static mrtjp.projectred.expansion.init.ExpansionItems.*;

public class ExpansionItemModelProvider extends ItemModelProvider {

    public ExpansionItemModelProvider(DataGenerator generator, ExistingFileHelper existingFileHelper) {
        super(generator, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Item Models";
    }

    @Override
    protected void registerModels() {

        simpleItemBlock(PROJECT_BENCH_BLOCK.get());
        simpleItemBlock(BATTERY_BOX_BLOCK.get());
        simpleItemBlock(AUTO_CRAFTER_BLOCK.get());
        simpleItemBlock(CHARGING_BENCH_BLOCK.get());
        simpleItemBlock(FIRE_STARTER_BLOCK.get());
        simpleItemBlock(FRAME_ACTUATOR_BLOCK.get());
        simpleItemBlock(TRANSPOSER_BLOCK.get());
        simpleItemBlock(BLOCK_BREAKER_BLOCK.get());
        simpleItemBlock(DEPLOYER_BLOCK.get());

        generated(FRAME_BLOCK.get()).noTexture();
        generated(FRAME_MOTOR_BLOCK.get()).noTexture();

        generated(EMPTY_BATTERY_ITEM);
        generated(BATTERY_ITEM);

        handheld(ELECTRIC_SCREWDRIVER_ITEM);

        writablePlanItem(RECIPE_PLAN_ITEM.get());

        chargeableItemBlock(BATTERY_BOX_BLOCK.get(), 8);
    }

    private void chargeableItemBlock(Block block, int chargeLevels) {

        ItemModelBuilder builder = getBuilder(block)
                .parent(new ModelFile.UncheckedModelFile(modLoc("block/" + name(block))));

        // Add overrides for each non-zero state
        for (int i = 1; i <= chargeLevels; i++) {
            builder.override()
                    .predicate(ITEM_MODEL_PROPERTY_CHARGE_LEVEL, i)
                    .model(getBuilder(name(block) + "_charge" + i)
                            .parent(new ModelFile.UncheckedModelFile(modLoc("block/" + name(block) + "_charge" + i))))
                    .end();
        }
    }

    private void writablePlanItem(Item item) {

        getBuilder(item)
                .parent(GENERATED)
                .texture("layer0", modLoc("item/" + name(item)))
                .override()
                    .predicate(ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN, 1)
                    .model(getBuilder(name(item) + "_written")
                            .texture("layer0", modLoc("item/" + name(item) + "_written"))
                            .parent(GENERATED))
                    .end();
    }
}
