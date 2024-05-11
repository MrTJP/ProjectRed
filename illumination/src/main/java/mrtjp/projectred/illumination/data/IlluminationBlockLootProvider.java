package mrtjp.projectred.illumination.data;

import codechicken.lib.datagen.LootTableProvider;
import mrtjp.projectred.illumination.BlockLightType;
import net.minecraft.data.DataGenerator;

import static mrtjp.projectred.illumination.init.IlluminationBlocks.ILLUMAR_SMART_LAMP;

public class IlluminationBlockLootProvider extends LootTableProvider.BlockLootProvider {

    public IlluminationBlockLootProvider(DataGenerator dataGenerator) {
        super(dataGenerator);
    }

    @Override
    public String getName() {
        return "ProjectRed-Illumination Block Loot Tables";
    }

    @Override
    protected void registerTables() {
        for (BlockLightType lampType : BlockLightType.values()) {
            for (int color = 0; color < 16; color++) {
                register(lampType.getBlock(color, false), singleItem(lampType.getBlock(color, false)));
                register(lampType.getBlock(color, true), singleItem(lampType.getBlock(color, true)));
            }
        }

        register(ILLUMAR_SMART_LAMP.get(), singleItem(ILLUMAR_SMART_LAMP.get()));
    }
}
