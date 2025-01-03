package mrtjp.projectred.core.init;

import mrtjp.projectred.core.block.ElectrotineGeneratorBlock;
import mrtjp.projectred.core.tile.ElectrotineGeneratorBlockEntity;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.core.ProjectRedCore.*;

@SuppressWarnings({ "DataFlowIssue", "NotNullFieldNotInitialized" })
public class CoreBlocks {

    public static final String ID_ELECTROTINE_GENERATOR = "electrotine_generator";

    // Blocks
    public static RegistryObject<Block> ELECTROTINE_GENERATOR_BLOCK;

    // Tile
    public static RegistryObject<BlockEntityType<ElectrotineGeneratorBlockEntity>> ELECTROTINE_GENERATOR_BLOCK_ENTITY;


    public static void register() {

        // Blocks
        ELECTROTINE_GENERATOR_BLOCK = BLOCKS.register(ID_ELECTROTINE_GENERATOR, ElectrotineGeneratorBlock::new);

        // Block Items
        ITEMS.register(ID_ELECTROTINE_GENERATOR, () -> new BlockItem(ELECTROTINE_GENERATOR_BLOCK.get(), new Item.Properties().tab(CORE_CREATIVE_TAB)));

        // Tiles
        ELECTROTINE_GENERATOR_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_ELECTROTINE_GENERATOR, () -> BlockEntityType.Builder.of(ElectrotineGeneratorBlockEntity::new, ELECTROTINE_GENERATOR_BLOCK.get()).build(null));
    }
}
