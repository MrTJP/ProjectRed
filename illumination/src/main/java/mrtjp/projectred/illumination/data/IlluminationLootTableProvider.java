package mrtjp.projectred.illumination.data;

import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.ProjectRedIllumination;
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

import static mrtjp.projectred.illumination.init.IlluminationBlocks.ILLUMAR_SMART_LAMP;

public class IlluminationLootTableProvider extends LootTableProvider {

    public IlluminationLootTableProvider(PackOutput output, CompletableFuture<HolderLookup.Provider> registries) {
        super(output, Set.of(), List.of(
                new LootTableProvider.SubProviderEntry(BlockLootTable::new, LootContextParamSets.BLOCK)),
                registries
        );
    }

    private static final class BlockLootTable extends BlockLootSubProvider {

        public BlockLootTable(HolderLookup.Provider provider) {
            super(Set.of(), FeatureFlags.REGISTRY.allFlags(), provider);
        }

        @Override
        protected Iterable<Block> getKnownBlocks() {
            return ProjectRedIllumination.BLOCKS.getEntries()
                    .stream()
                    .map(Holder::value)
                    .toList();
        }

        @Override
        protected void generate() {
            for (BlockLightType lampType : BlockLightType.values()) {
                for (int color = 0; color < 16; color++) {
                    dropSelf(lampType.getBlock(color, false));
                    dropSelf(lampType.getBlock(color, true));
                }
            }

            dropSelf(ILLUMAR_SMART_LAMP.get());
        }
    }
}
