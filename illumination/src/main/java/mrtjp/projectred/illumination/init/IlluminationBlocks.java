package mrtjp.projectred.illumination.init;

import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.block.IllumarSmartLampBlock;
import mrtjp.projectred.illumination.tile.IllumarSmartLampBlockEntity;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;

import java.util.function.Supplier;

import static mrtjp.projectred.illumination.ProjectRedIllumination.*;

@SuppressWarnings({ "DataFlowIssue", "NotNullFieldNotInitialized" })
public class IlluminationBlocks {

    public static final String ID_ILLUMAR_SMART_LAMP = "illumar_smart_lamp";

    // Blocks
    public static Supplier<Block> ILLUMAR_SMART_LAMP;

    // Block Entities
    public static Supplier<BlockEntityType<IllumarSmartLampBlockEntity>> ILLUMAR_SMART_LAMP_BLOCK_ENTITY;


    public static void register() {

        // Block lights
        for (BlockLightType lampType : BlockLightType.values()) {
            lampType.registerBlocks(BLOCKS, ITEMS, BLOCK_ENTITY_TYPES);
        }

        // Blocks
        ILLUMAR_SMART_LAMP = BLOCKS.register(ID_ILLUMAR_SMART_LAMP, IllumarSmartLampBlock::new);

        // Block Items
        ITEMS.register(ID_ILLUMAR_SMART_LAMP, () -> new BlockItem(ILLUMAR_SMART_LAMP.get(), new Item.Properties()));

        // Block Entities
        ILLUMAR_SMART_LAMP_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_ILLUMAR_SMART_LAMP, () -> BlockEntityType.Builder.of(IllumarSmartLampBlockEntity::new, ILLUMAR_SMART_LAMP.get()).build(null));
    }
}
