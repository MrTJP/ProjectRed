package mrtjp.projectred.core.init;

import mrtjp.projectred.core.block.ElectrotineGeneratorBlock;
import mrtjp.projectred.core.tile.ElectrotineGeneratorBlockEntity;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;

import java.util.function.Supplier;

import static mrtjp.projectred.core.ProjectRedCore.*;

@SuppressWarnings({ "DataFlowIssue", "NotNullFieldNotInitialized" })
public class CoreBlocks {

    public static final String ID_ELECTROTINE_GENERATOR = "electrotine_generator";

    // Blocks
    public static Supplier<Block> ELECTROTINE_GENERATOR_BLOCK;

    // Tile
    public static Supplier<BlockEntityType<ElectrotineGeneratorBlockEntity>> ELECTROTINE_GENERATOR_BLOCK_ENTITY;


    public static void register() {

        // Blocks
        ELECTROTINE_GENERATOR_BLOCK = BLOCKS.register(ID_ELECTROTINE_GENERATOR, ElectrotineGeneratorBlock::new);

        // Block Items
        ITEMS.register(ID_ELECTROTINE_GENERATOR, () -> new BlockItem(ELECTROTINE_GENERATOR_BLOCK.get(), new Item.Properties()));

        // Tiles
        ELECTROTINE_GENERATOR_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_ELECTROTINE_GENERATOR, () -> BlockEntityType.Builder.of(ElectrotineGeneratorBlockEntity::new, ELECTROTINE_GENERATOR_BLOCK.get()).build(null));
    }

    public static void registerCaps(RegisterCapabilitiesEvent event) {
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK, ELECTROTINE_GENERATOR_BLOCK_ENTITY.get(), (tile, ctx) -> tile.getHandler());
    }
}
