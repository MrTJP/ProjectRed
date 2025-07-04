package mrtjp.projectred.core.data;

import mrtjp.projectred.core.ProjectRedCore;
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

import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;

public class CoreLootTableProvider extends LootTableProvider {

    public CoreLootTableProvider(PackOutput output, CompletableFuture<HolderLookup.Provider> registries) {
        super(output, Set.of(), List.of(
                new LootTableProvider.SubProviderEntry(BlockLootTables::new, LootContextParamSets.BLOCK)
        ), registries);
    }

    private static final class BlockLootTables extends BlockLootSubProvider {

        BlockLootTables(HolderLookup.Provider provider) {
            super(Set.of(), FeatureFlags.REGISTRY.allFlags(), provider);
        }

        @Override
        protected Iterable<Block> getKnownBlocks() {
            return ProjectRedCore.BLOCKS.getEntries()
                    .stream()
                    .map(Holder::value)
                    .toList();
        }

        @Override
        protected void generate() {
            dropSelf(ELECTROTINE_GENERATOR_BLOCK.get());
        }
    }
}
