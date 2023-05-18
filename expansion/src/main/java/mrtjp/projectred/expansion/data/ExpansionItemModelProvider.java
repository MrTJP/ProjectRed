package mrtjp.projectred.expansion.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.world.level.block.Block;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.item.Item;
import net.minecraftforge.client.model.generators.ItemModelBuilder;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionClientInit.ITEM_MODEL_PROPERTY_CHARGE_LEVEL;
import static mrtjp.projectred.expansion.init.ExpansionClientInit.ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

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

        simpleItemBlock(PROJECT_BENCH_BLOCK);
        simpleItemBlock(BATTERY_BOX_BLOCK);
        simpleItemBlock(AUTO_CRAFTER_BLOCK);
        simpleItemBlock(CHARGING_BENCH_BLOCK);
        simpleItemBlock(FIRE_STARTER_BLOCK);

        generated(EMPTY_BATTERY_ITEM);
        generated(BATTERY_ITEM);

        handheld(ELECTRIC_SCREWDRIVER_ITEM);

        writablePlanItem(RECIPE_PLAN_ITEM);

        chargeableItemBlock(BATTERY_BOX_BLOCK, 8);
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
