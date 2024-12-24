package mrtjp.projectred.fabrication.data;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import net.minecraft.core.Holder;
import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.BlockLootSubProvider;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.flag.FeatureFlags;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;

import java.util.List;
import java.util.Set;

import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

public class FabricationLootTableProvider extends LootTableProvider {

    public FabricationLootTableProvider(PackOutput output) {
        super(output, Set.of(), List.of(
                new LootTableProvider.SubProviderEntry(BlockLootTables::new, LootContextParamSets.BLOCK)
        ));
    }

    private static final class BlockLootTables extends BlockLootSubProvider {

        BlockLootTables() {
            super(Set.of(), FeatureFlags.REGISTRY.allFlags());
        }

        @Override
        protected Iterable<Block> getKnownBlocks() {
            return ProjectRedFabrication.BLOCKS.getEntries()
                    .stream()
                    .map(Holder::value)
                    .toList();
        }

        @Override
        protected void generate() {
            dropSelf(IC_WORKBENCH_BLOCK.get());
            dropSelf(PLOTTING_TABLE_BLOCK.get());
            dropSelf(LITHOGRAPHY_TABLE_BLOCK.get());
            dropSelf(PACKAGING_TABLE_BLOCK.get());
        }
    }
}
