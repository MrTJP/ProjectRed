package mrtjp.projectred.expansion.data;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.BlockLootSubProvider;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.flag.FeatureFlags;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;

import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;

public class ExpansionLootTableProvider extends LootTableProvider {

    public ExpansionLootTableProvider(CompletableFuture<HolderLookup.Provider> registries, PackOutput output) {
        super(output, Set.of(), List.of(
                new LootTableProvider.SubProviderEntry(BlockLootTable::new, LootContextParamSets.BLOCK)
        ), registries);
    }

    private static final class BlockLootTable extends BlockLootSubProvider {
        BlockLootTable(HolderLookup.Provider provider) {
            super(Set.of(), FeatureFlags.REGISTRY.allFlags(), provider);
        }

        @Override
        protected Iterable<Block> getKnownBlocks() {
            return ProjectRedExpansion.BLOCKS.getEntries()
                    .stream()
                    .map(Holder::value)
                    .toList();
        }

        @Override
        protected void generate() {
            dropSelf(PROJECT_BENCH_BLOCK.get());
            dropSelf(BATTERY_BOX_BLOCK.get());
            dropSelf(AUTO_CRAFTER_BLOCK.get());
            dropSelf(CHARGING_BENCH_BLOCK.get());
            dropSelf(FIRE_STARTER_BLOCK.get());
            dropSelf(FRAME_BLOCK.get());
            dropSelf(FRAME_MOTOR_BLOCK.get());
            dropSelf(FRAME_ACTUATOR_BLOCK.get());
            dropSelf(TRANSPOSER_BLOCK.get());
            dropSelf(BLOCK_BREAKER_BLOCK.get());
            dropSelf(DEPLOYER_BLOCK.get());
        }
    }
}
