package mrtjp.projectred.core.init;

import mrtjp.projectred.core.block.ElectrotineGeneratorBlock;
import mrtjp.projectred.core.tile.ElectrotineGeneratorTile;
import net.minecraft.item.BlockItem;
import net.minecraft.item.Item;
import net.minecraft.tileentity.TileEntityType;

import static mrtjp.projectred.ProjectRedCore.*;
import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_GENERATOR_BLOCK;

public class CoreBlocks {

    public static final String ID_ELECTROTINE_GENERATOR = "electrotine_generator";

    public static void register() {

        // Blocks
        BLOCKS.register(ID_ELECTROTINE_GENERATOR, ElectrotineGeneratorBlock::new);

        // Block Items
        ITEMS.register(ID_ELECTROTINE_GENERATOR, () -> new BlockItem(ELECTROTINE_GENERATOR_BLOCK, new Item.Properties().tab(CORE_GROUP)));

        // Tiles
        TILE_ENTITIES.register(ID_ELECTROTINE_GENERATOR, () -> TileEntityType.Builder.of(ElectrotineGeneratorTile::new, ELECTROTINE_GENERATOR_BLOCK).build(null));
    }
}
