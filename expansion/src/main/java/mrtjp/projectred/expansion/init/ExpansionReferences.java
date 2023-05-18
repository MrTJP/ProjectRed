package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.inventory.container.AutoCrafterContainer;
import mrtjp.projectred.expansion.inventory.container.BatteryBoxContainer;
import mrtjp.projectred.expansion.inventory.container.ChargingBenchContainer;
import mrtjp.projectred.expansion.inventory.container.ProjectBenchContainer;
import net.minecraft.block.Block;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.tileentity.TileEntityType;
import net.minecraftforge.registries.ObjectHolder;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;
import static mrtjp.projectred.expansion.init.ExpansionItems.*;

@ObjectHolder(MOD_ID)
public class ExpansionReferences {

    // Blocks
    @ObjectHolder(ID_PROJECT_BENCH)
    public static Block PROJECT_BENCH_BLOCK = null;
    @ObjectHolder(ID_BATTERY_BOX)
    public static Block BATTERY_BOX_BLOCK = null;
    @ObjectHolder(ID_CHARGING_BENCH)
    public static Block CHARGING_BENCH_BLOCK = null;
    @ObjectHolder(ID_AUTO_CRAFTER)
    public static Block AUTO_CRAFTER_BLOCK = null;
    @ObjectHolder(ID_FIRE_STARTER)
    public static Block FIRE_STARTER_BLOCK = null;
//    @ObjectHolder(ID_INDUCTION_FURNACE)
//    public static Block INDUCTION_FURNACE_BLOCK = null;
//    @ObjectHolder(ID_TELEPOSER)
//    public static Block TELEPOSER_BLOCK = null;

    // Tiles
    @ObjectHolder(ID_PROJECT_BENCH)
    public static TileEntityType<?> PROJECT_BENCH_TILE = null;
    @ObjectHolder(ID_BATTERY_BOX)
    public static TileEntityType<?> BATTERY_BOX_TILE = null;
    @ObjectHolder(ID_CHARGING_BENCH)
    public static TileEntityType<?> CHARGING_BENCH_TILE = null;
    @ObjectHolder(ID_AUTO_CRAFTER)
    public static TileEntityType<?> AUTO_CRAFTER_TILE = null;
    @ObjectHolder(ID_FIRE_STARTER)
    public static TileEntityType<?> FIRE_STARTER_TILE = null;
//    @ObjectHolder(ID_INDUCTION_FURNACE)
//    public static TileEntityType<?> INDUCTION_FURNACE_TILE = null;
//    @ObjectHolder(ID_TELEPOSER)
//    public static TileEntityType<?> TELEPOSER_TILE = null;

    // Containers
    @ObjectHolder(ID_PROJECT_BENCH)
    public static ContainerType<ProjectBenchContainer> PROJECT_BENCH_CONTAINER = null;
    @ObjectHolder(ID_BATTERY_BOX)
    public static ContainerType<BatteryBoxContainer> BATTERY_BOX_CONTAINER = null;
    @ObjectHolder(ID_AUTO_CRAFTER)
    public static ContainerType<AutoCrafterContainer> AUTO_CRAFTER_CONTAINER = null;
    @ObjectHolder(ID_CHARGING_BENCH)
    public static ContainerType<ChargingBenchContainer> CHARGING_BENCH_CONTAINER = null;

    // Items
    @ObjectHolder(ID_RECIPE_PLAN)
    public static Item RECIPE_PLAN_ITEM = null;
    @ObjectHolder(ID_BATTERY)
    public static Item BATTERY_ITEM = null;
    @ObjectHolder(ID_EMPTY_BATTERY)
    public static Item EMPTY_BATTERY_ITEM = null;
    @ObjectHolder(ID_ELECTRIC_SCREWDRIVER)
    public static Item ELECTRIC_SCREWDRIVER_ITEM = null;
}
