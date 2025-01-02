package mrtjp.projectred.exploration.data;

import mrtjp.projectred.exploration.ProjectRedExploration;
import net.minecraft.core.Holder;
import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.BlockLootSubProvider;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.flag.FeatureFlags;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.ItemLike;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.entries.LootItem;
import net.minecraft.world.level.storage.loot.functions.ApplyBonusCount;
import net.minecraft.world.level.storage.loot.functions.SetItemCountFunction;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.providers.number.UniformGenerator;

import java.util.List;
import java.util.Set;

import static mrtjp.projectred.core.init.CoreItems.*;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationItems.RAW_SILVER_ITEM;
import static mrtjp.projectred.exploration.init.ExplorationItems.RAW_TIN_ITEM;

public class ExplorationLootTableProvider extends LootTableProvider {

    public ExplorationLootTableProvider(PackOutput output) {
        super(output, Set.of(), List.of(
                new LootTableProvider.SubProviderEntry(BlockLootTable::new, LootContextParamSets.BLOCK)
        ));
    }

    private static final class BlockLootTable extends BlockLootSubProvider {

        BlockLootTable() {
            super(Set.of(), FeatureFlags.REGISTRY.allFlags());
        }

        @Override
        protected Iterable<Block> getKnownBlocks() {
            return ProjectRedExploration.BLOCKS.getEntries()
                    .stream()
                    .map(Holder::value)
                    .toList();
        }

        @Override
        protected void generate() {
            // Ores
            add(RUBY_ORE_BLOCK.get(),                  b -> createOreMultiDrops(b, RUBY_ITEM.get(), 1, 2));
            add(DEEPSLATE_RUBY_ORE_BLOCK.get(),        b -> createOreMultiDrops(b, RUBY_ITEM.get(), 1, 2));
            add(SAPPHIRE_ORE_BLOCK.get(),              b -> createOreMultiDrops(b, SAPPHIRE_ITEM.get(), 1, 2));
            add(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get(),    b -> createOreMultiDrops(b, SAPPHIRE_ITEM.get(), 1, 2));
            add(PERIDOT_ORE_BLOCK.get(),               b -> createOreMultiDrops(b, PERIDOT_ITEM.get(), 1, 2));
            add(DEEPSLATE_PERIDOT_ORE_BLOCK.get(),     b -> createOreMultiDrops(b, PERIDOT_ITEM.get(), 1, 2));
            add(ELECTROTINE_ORE_BLOCK.get(),           b -> createOreMultiDrops(b, ELECTROTINE_DUST_ITEM.get(), 1, 8));
            add(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get(), b -> createOreMultiDrops(b, ELECTROTINE_DUST_ITEM.get(), 1, 8));
            add(TIN_ORE_BLOCK.get(),                   b -> createOreDrop(b, RAW_TIN_ITEM.get()));
            add(DEEPSLATE_TIN_ORE_BLOCK.get(),         b -> createOreDrop(b, RAW_TIN_ITEM.get()));
            add(SILVER_ORE_BLOCK.get(),                b -> createOreDrop(b, RAW_SILVER_ITEM.get()));
            add(DEEPSLATE_SILVER_ORE_BLOCK.get(),      b -> createOreDrop(b, RAW_SILVER_ITEM.get()));

            // Decorative Blocks
            dropSelf(MARBLE_BLOCK.get());
            dropSelf(MARBLE_BRICK_BLOCK.get());
            dropSelf(BASALT_BLOCK.get());
            dropSelf(BASALT_COBBLE_BLOCK.get());
            dropSelf(BASALT_BRICK_BLOCK.get());
            dropSelf(RUBY_BLOCK.get());
            dropSelf(SAPPHIRE_BLOCK.get());
            dropSelf(PERIDOT_BLOCK.get());
            dropSelf(ELECTROTINE_BLOCK.get());
            dropSelf(RAW_TIN_BLOCK.get());
            dropSelf(RAW_SILVER_BLOCK.get());
            dropSelf(TIN_BLOCK.get());
            dropSelf(SILVER_BLOCK.get());

            // Walls
            dropSelf(MARBLE_WALL.get());
            dropSelf(MARBLE_BRICK_WALL.get());
            dropSelf(BASALT_WALL.get());
            dropSelf(BASALT_COBBLE_WALL.get());
            dropSelf(BASALT_BRICK_WALL.get());
            dropSelf(RUBY_BLOCK_WALL.get());
            dropSelf(SAPPHIRE_BLOCK_WALL.get());
            dropSelf(PERIDOT_BLOCK_WALL.get());
            dropSelf(ELECTROTINE_BLOCK_WALL.get());
        }

        private LootTable.Builder createOreMultiDrops(Block oreBlock, ItemLike dropItem, float min, float max) {
            return createSilkTouchDispatchTable(
                    oreBlock,
                    this.applyExplosionDecay(
                            oreBlock,
                            LootItem.lootTableItem(dropItem)
                                    .apply(SetItemCountFunction.setCount(UniformGenerator.between(min, max)))
                                    .apply(ApplyBonusCount.addOreBonusCount(Enchantments.BLOCK_FORTUNE))
                    )
            );
        }
    }
}
