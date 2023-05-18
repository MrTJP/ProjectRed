package mrtjp.projectred.core.init;

import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorContainer;
import mrtjp.projectred.core.tile.ElectrotineGeneratorTile;
import net.minecraft.block.Block;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.tileentity.TileEntityType;
import net.minecraftforge.registries.ObjectHolder;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreBlocks.ID_ELECTROTINE_GENERATOR;
import static mrtjp.projectred.core.init.CoreItems.*;

@ObjectHolder(MOD_ID)
public class CoreReferences {

    // Blocks
    @ObjectHolder(ID_ELECTROTINE_GENERATOR)
    public static Block ELECTROTINE_GENERATOR_BLOCK = null;

    // Containers
    @ObjectHolder(ID_ELECTROTINE_GENERATOR)
    public static ContainerType<ElectrotineGeneratorContainer> ELECTROTINE_GENERATOR_CONTAINER = null;

    // Tile
    @ObjectHolder(ID_ELECTROTINE_GENERATOR)
    public static TileEntityType<ElectrotineGeneratorTile> ELECTROTINE_GENERATOR_TILE = null;

    // Items
    @ObjectHolder(ID_PLATE)
    public static Item PLATE_ITEM;
    @ObjectHolder(ID_CONDUCTIVE_PLATE)
    public static Item CONDUCTIVE_PLATE_ITEM;
    @ObjectHolder(ID_WIRED_PLATE)
    public static Item WIRED_PLATE_ITEM;
    @ObjectHolder(ID_BUNDLED_PLATE)
    public static Item BUNDLED_PLATE_ITEM;
    @ObjectHolder(ID_PLATFORMED_PLATE)
    public static Item PLATFORMED_PLATE_ITEM;
    @ObjectHolder(ID_ANODE)
    public static Item ANODE_ITEM;
    @ObjectHolder(ID_CATHODE)
    public static Item CATHODE_ITEM;
    @ObjectHolder(ID_POINTER)
    public static Item POINTER_ITEM;
    @ObjectHolder(ID_SILICON_CHIP)
    public static Item SILICON_CHIP_ITEM;
    @ObjectHolder(ID_ENERGIZED_SILICON_CHIP)
    public static Item ENERGIZED_SILICON_CHIP_ITEM;
    @ObjectHolder(ID_COPPER_INGOT)
    public static Item COPPER_INGOT_ITEM;
    @ObjectHolder(ID_TIN_INGOT)
    public static Item TIN_INGOT_ITEM;
    @ObjectHolder(ID_SILVER_INGOT)
    public static Item SILVER_INGOT_ITEM;
    @ObjectHolder(ID_RED_ALLOY_INGOT)
    public static Item RED_ALLOY_INGOT_ITEM;
    @ObjectHolder(ID_ELECTROTINE_ALLOY_INGOT)
    public static Item ELECTROTINE_ALLOY_INGOT_ITEM;
    @ObjectHolder(ID_ELECTROTINE_DUST)
    public static Item ELECTROTINE_DUST_ITEM;
    @ObjectHolder(ID_RUBY)
    public static Item RUBY_ITEM;
    @ObjectHolder(ID_SAPPHIRE)
    public static Item SAPPHIRE_ITEM;
    @ObjectHolder(ID_PERIDOT)
    public static Item PERIDOT_ITEM;
    @ObjectHolder(ID_SAND_COAL_COMP)
    public static Item SAND_COAL_COMP_ITEM;
    @ObjectHolder(ID_RED_IRON_COMP)
    public static Item RED_IRON_COMP_ITEM;
    @ObjectHolder(ID_ELECTROTINE_IRON_COMP)
    public static Item ELECTROTINE_IRON_COMP_ITEM;
    @ObjectHolder(ID_SILICON_BOULE)
    public static Item SILICON_BOULE_ITEM;
    @ObjectHolder(ID_SILICON)
    public static Item SILICON_ITEM;
    @ObjectHolder(ID_RED_SILICON_COMP)
    public static Item RED_SILICON_COMP_ITEM;
    @ObjectHolder(ID_GLOW_SILICON_COMP)
    public static Item GLOW_SILICON_COMP_ITEM;
    @ObjectHolder(ID_ELECTROTINE_SILICON_COMP)
    public static Item ELECTROTINE_SILICON_COMP_ITEM;
    @ObjectHolder(ID_INFUSED_SILICON)
    public static Item INFUSED_SILICON_ITEM;
    @ObjectHolder(ID_ENERGIZED_SILICON)
    public static Item ENERGIZED_SILICON_ITEM;
    @ObjectHolder(ID_ELECTROTINE_SILICON)
    public static Item ELECTROTINE_SILICON_ITEM;
    @ObjectHolder(ID_COPPER_COIL)
    public static Item COPPER_COIL_ITEM;
    @ObjectHolder(ID_IRON_COIL)
    public static Item IRON_COIL_ITEM;
    @ObjectHolder(ID_GOLD_COIL)
    public static Item GOLD_COIL_ITEM;
    @ObjectHolder(ID_MOTOR)
    public static Item MOTOR_ITEM;
    @ObjectHolder(ID_WOVEN_CLOTH)
    public static Item WOVEN_CLOTH_ITEM;
    @ObjectHolder(ID_SAIL)
    public static Item SAIL_ITEM;
    @ObjectHolder(ID_WHITE_ILLUMAR)
    public static Item WHITE_ILLUMAR_ITEM;
    @ObjectHolder(ID_ORANGE_ILLUMAR)
    public static Item ORANGE_ILLUMAR_ITEM;
    @ObjectHolder(ID_MAGENTA_ILLUMAR)
    public static Item MAGENTA_ILLUMAR_ITEM;
    @ObjectHolder(ID_LIGHT_BLUE_ILLUMAR)
    public static Item LIGHT_BLUE_ILLUMAR_ITEM;
    @ObjectHolder(ID_YELLOW_ILLUMAR)
    public static Item YELLOW_ILLUMAR_ITEM;
    @ObjectHolder(ID_LIME_ILLUMAR)
    public static Item LIME_ILLUMAR_ITEM;
    @ObjectHolder(ID_PINK_ILLUMAR)
    public static Item PINK_ILLUMAR_ITEM;
    @ObjectHolder(ID_GRAY_ILLUMAR)
    public static Item GRAY_ILLUMAR_ITEM;
    @ObjectHolder(ID_LIGHT_GRAY_ILLUMAR)
    public static Item LIGHT_GRAY_ILLUMAR_ITEM;
    @ObjectHolder(ID_CYAN_ILLUMAR)
    public static Item CYAN_ILLUMAR_ITEM;
    @ObjectHolder(ID_PURPLE_ILLUMAR)
    public static Item PURPLE_ILLUMAR_ITEM;
    @ObjectHolder(ID_BLUE_ILLUMAR)
    public static Item BLUE_ILLUMAR_ITEM;
    @ObjectHolder(ID_BROWN_ILLUMAR)
    public static Item BROWN_ILLUMAR_ITEM;
    @ObjectHolder(ID_GREEN_ILLUMAR)
    public static Item GREEN_ILLUMAR_ITEM;
    @ObjectHolder(ID_RED_ILLUMAR)
    public static Item RED_ILLUMAR_ITEM;
    @ObjectHolder(ID_BLACK_ILLUMAR)
    public static Item BLACK_ILLUMAR_ITEM;

    @ObjectHolder(ID_DRAW_PLATE)
    public static Item DRAW_PLATE_ITEM;
    @ObjectHolder(ID_SCREWDRIVER)
    public static Item SCREWDRIVER_ITEM;
    @ObjectHolder(ID_MULTIMETER)
    public static Item MULTIMETER_ITEM;
}
