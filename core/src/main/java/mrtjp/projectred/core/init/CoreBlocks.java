package mrtjp.projectred.core.init;

import mrtjp.projectred.core.block.ElectrotineGeneratorBlock;
import mrtjp.projectred.core.tile.ElectrotineGeneratorTile;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.entity.BlockEntityType;

import static mrtjp.projectred.core.ProjectRedCore.*;
import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_GENERATOR_BLOCK;

@SuppressWarnings("DataFlowIssue")
public class CoreBlocks {

    public static final String ID_ELECTROTINE_GENERATOR = "electrotine_generator";

    public static void register() {

        // Blocks
        BLOCKS.register(ID_ELECTROTINE_GENERATOR, ElectrotineGeneratorBlock::new);

        // Block Items
        ITEMS.register(ID_ELECTROTINE_GENERATOR, () -> new BlockItem(ELECTROTINE_GENERATOR_BLOCK, new Item.Properties().tab(CORE_CREATIVE_TAB)));

        // Tiles
        BLOCK_ENTITIES.register(ID_ELECTROTINE_GENERATOR, () -> BlockEntityType.Builder.of(ElectrotineGeneratorTile::new, ELECTROTINE_GENERATOR_BLOCK).build(null));
    }
}
