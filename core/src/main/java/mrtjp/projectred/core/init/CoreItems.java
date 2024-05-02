package mrtjp.projectred.core.init;

import mrtjp.projectred.core.item.DrawPlateItem;
import mrtjp.projectred.core.item.MultimeterItem;
import mrtjp.projectred.core.item.ScrewdriverItem;
import net.minecraft.world.item.Item;
import net.minecraftforge.registries.RegistryObject;

import java.util.function.Supplier;

import static mrtjp.projectred.core.ProjectRedCore.ITEMS;

@SuppressWarnings("NotNullFieldNotInitialized")
public class CoreItems {

    public static final String ID_PLATE = "plate";
    public static final String ID_CONDUCTIVE_PLATE = "conductive_plate";
    public static final String ID_WIRED_PLATE = "wired_plate";
    public static final String ID_BUNDLED_PLATE = "bundled_plate";
    public static final String ID_PLATFORMED_PLATE = "platformed_plate";
    public static final String ID_ANODE = "anode";
    public static final String ID_CATHODE = "cathode";
    public static final String ID_POINTER = "pointer";
    public static final String ID_SILICON_CHIP = "silicon_chip";
    public static final String ID_ENERGIZED_SILICON_CHIP = "energized_silicon_chip";
    public static final String ID_RED_ALLOY_INGOT = "red_ingot";
    public static final String ID_ELECTROTINE_ALLOY_INGOT = "electrotine_ingot";
    public static final String ID_ELECTROTINE_DUST = "electrotine_dust";
    public static final String ID_RUBY = "ruby";
    public static final String ID_SAPPHIRE = "sapphire";
    public static final String ID_PERIDOT = "peridot";
    public static final String ID_SAND_COAL_COMP = "sand_coal_comp";
    public static final String ID_RED_IRON_COMP = "red_iron_comp";
    public static final String ID_ELECTROTINE_IRON_COMP = "electrotine_iron_comp";
    public static final String ID_SILICON_BOULE = "boule";
    public static final String ID_SILICON = "silicon";
    public static final String ID_RED_SILICON_COMP = "red_silicon_comp";
    public static final String ID_GLOW_SILICON_COMP = "glow_silicon_comp";
    public static final String ID_ELECTROTINE_SILICON_COMP = "electrotine_silicon_comp";
    public static final String ID_INFUSED_SILICON = "infused_silicon";
    public static final String ID_ENERGIZED_SILICON = "energized_silicon";
    public static final String ID_ELECTROTINE_SILICON = "electrotine_silicon";
    public static final String ID_COPPER_COIL = "copper_coil";
    public static final String ID_IRON_COIL = "iron_coil";
    public static final String ID_GOLD_COIL = "gold_coil";
    public static final String ID_MOTOR = "motor";
    public static final String ID_WOVEN_CLOTH = "woven_cloth";
    public static final String ID_SAIL = "sail";
    public static final String ID_WHITE_ILLUMAR = "white_illumar";
    public static final String ID_ORANGE_ILLUMAR = "orange_illumar";
    public static final String ID_MAGENTA_ILLUMAR = "magenta_illumar";
    public static final String ID_LIGHT_BLUE_ILLUMAR = "light_blue_illumar";
    public static final String ID_YELLOW_ILLUMAR = "yellow_illumar";
    public static final String ID_LIME_ILLUMAR = "lime_illumar";
    public static final String ID_PINK_ILLUMAR = "pink_illumar";
    public static final String ID_GRAY_ILLUMAR = "gray_illumar";
    public static final String ID_LIGHT_GRAY_ILLUMAR = "light_gray_illumar";
    public static final String ID_CYAN_ILLUMAR = "cyan_illumar";
    public static final String ID_PURPLE_ILLUMAR = "purple_illumar";
    public static final String ID_BLUE_ILLUMAR = "blue_illumar";
    public static final String ID_BROWN_ILLUMAR = "brown_illumar";
    public static final String ID_GREEN_ILLUMAR = "green_illumar";
    public static final String ID_RED_ILLUMAR = "red_illumar";
    public static final String ID_BLACK_ILLUMAR = "black_illumar";

    public static final String ID_DRAW_PLATE = "draw_plate";
    public static final String ID_SCREWDRIVER = "screwdriver";
    public static final String ID_MULTIMETER = "multimeter";

    // Ingots / dusts / gems
    public static RegistryObject<Item> RED_ALLOY_INGOT_ITEM;
    public static RegistryObject<Item> ELECTROTINE_ALLOY_INGOT_ITEM;
    public static RegistryObject<Item> ELECTROTINE_DUST_ITEM;
    public static RegistryObject<Item> RUBY_ITEM;
    public static RegistryObject<Item> SAPPHIRE_ITEM;
    public static RegistryObject<Item> PERIDOT_ITEM;

    // Recipe ingredients
    public static RegistryObject<Item> PLATE_ITEM;
    public static RegistryObject<Item> CONDUCTIVE_PLATE_ITEM;
    public static RegistryObject<Item> WIRED_PLATE_ITEM;
    public static RegistryObject<Item> BUNDLED_PLATE_ITEM;
    public static RegistryObject<Item> PLATFORMED_PLATE_ITEM;
    public static RegistryObject<Item> ANODE_ITEM;
    public static RegistryObject<Item> CATHODE_ITEM;
    public static RegistryObject<Item> POINTER_ITEM;
    public static RegistryObject<Item> SILICON_CHIP_ITEM;
    public static RegistryObject<Item> ENERGIZED_SILICON_CHIP_ITEM;
    public static RegistryObject<Item> SAND_COAL_COMP_ITEM;
    public static RegistryObject<Item> RED_IRON_COMP_ITEM;
    public static RegistryObject<Item> ELECTROTINE_IRON_COMP_ITEM;
    public static RegistryObject<Item> SILICON_BOULE_ITEM;
    public static RegistryObject<Item> SILICON_ITEM;
    public static RegistryObject<Item> RED_SILICON_COMP_ITEM;
    public static RegistryObject<Item> GLOW_SILICON_COMP_ITEM;
    public static RegistryObject<Item> ELECTROTINE_SILICON_COMP_ITEM;
    public static RegistryObject<Item> INFUSED_SILICON_ITEM;
    public static RegistryObject<Item> ENERGIZED_SILICON_ITEM;
    public static RegistryObject<Item> ELECTROTINE_SILICON_ITEM;
    public static RegistryObject<Item> COPPER_COIL_ITEM;
    public static RegistryObject<Item> IRON_COIL_ITEM;
    public static RegistryObject<Item> GOLD_COIL_ITEM;
    public static RegistryObject<Item> MOTOR_ITEM;
    public static RegistryObject<Item> WOVEN_CLOTH_ITEM;
    public static RegistryObject<Item> SAIL_ITEM;

    // Illumars
    public static RegistryObject<Item> WHITE_ILLUMAR_ITEM;
    public static RegistryObject<Item> ORANGE_ILLUMAR_ITEM;
    public static RegistryObject<Item> MAGENTA_ILLUMAR_ITEM;
    public static RegistryObject<Item> LIGHT_BLUE_ILLUMAR_ITEM;
    public static RegistryObject<Item> YELLOW_ILLUMAR_ITEM;
    public static RegistryObject<Item> LIME_ILLUMAR_ITEM;
    public static RegistryObject<Item> PINK_ILLUMAR_ITEM;
    public static RegistryObject<Item> GRAY_ILLUMAR_ITEM;
    public static RegistryObject<Item> LIGHT_GRAY_ILLUMAR_ITEM;
    public static RegistryObject<Item> CYAN_ILLUMAR_ITEM;
    public static RegistryObject<Item> PURPLE_ILLUMAR_ITEM;
    public static RegistryObject<Item> BLUE_ILLUMAR_ITEM;
    public static RegistryObject<Item> BROWN_ILLUMAR_ITEM;
    public static RegistryObject<Item> GREEN_ILLUMAR_ITEM;
    public static RegistryObject<Item> RED_ILLUMAR_ITEM;
    public static RegistryObject<Item> BLACK_ILLUMAR_ITEM;

    // Tools
    public static RegistryObject<Item> DRAW_PLATE_ITEM;
    public static RegistryObject<Item> SCREWDRIVER_ITEM;
    public static RegistryObject<Item> MULTIMETER_ITEM;

    public static void register() {

        // Ingots/dusts/gems
        RED_ALLOY_INGOT_ITEM = ITEMS.register(ID_RED_ALLOY_INGOT, createSimpleItemSupplier());
        ELECTROTINE_ALLOY_INGOT_ITEM = ITEMS.register(ID_ELECTROTINE_ALLOY_INGOT, createSimpleItemSupplier());
        ELECTROTINE_DUST_ITEM = ITEMS.register(ID_ELECTROTINE_DUST, createSimpleItemSupplier());
        RUBY_ITEM = ITEMS.register(ID_RUBY, createSimpleItemSupplier());
        SAPPHIRE_ITEM = ITEMS.register(ID_SAPPHIRE, createSimpleItemSupplier());
        PERIDOT_ITEM = ITEMS.register(ID_PERIDOT, createSimpleItemSupplier());

        // Recipe ingredients
        PLATE_ITEM = ITEMS.register(ID_PLATE, createSimpleItemSupplier());
        CONDUCTIVE_PLATE_ITEM = ITEMS.register(ID_CONDUCTIVE_PLATE, createSimpleItemSupplier());
        WIRED_PLATE_ITEM = ITEMS.register(ID_WIRED_PLATE, createSimpleItemSupplier());
        BUNDLED_PLATE_ITEM = ITEMS.register(ID_BUNDLED_PLATE, createSimpleItemSupplier());
        PLATFORMED_PLATE_ITEM = ITEMS.register(ID_PLATFORMED_PLATE, createSimpleItemSupplier());
        ANODE_ITEM = ITEMS.register(ID_ANODE, createSimpleItemSupplier());
        CATHODE_ITEM = ITEMS.register(ID_CATHODE, createSimpleItemSupplier());
        POINTER_ITEM = ITEMS.register(ID_POINTER, createSimpleItemSupplier());
        SILICON_CHIP_ITEM = ITEMS.register(ID_SILICON_CHIP, createSimpleItemSupplier());
        ENERGIZED_SILICON_CHIP_ITEM = ITEMS.register(ID_ENERGIZED_SILICON_CHIP, createSimpleItemSupplier());
        SAND_COAL_COMP_ITEM = ITEMS.register(ID_SAND_COAL_COMP, createSimpleItemSupplier());
        RED_IRON_COMP_ITEM = ITEMS.register(ID_RED_IRON_COMP, createSimpleItemSupplier());
        ELECTROTINE_IRON_COMP_ITEM = ITEMS.register(ID_ELECTROTINE_IRON_COMP, createSimpleItemSupplier());
        SILICON_BOULE_ITEM = ITEMS.register(ID_SILICON_BOULE, createSimpleItemSupplier());
        SILICON_ITEM = ITEMS.register(ID_SILICON, createSimpleItemSupplier());
        RED_SILICON_COMP_ITEM = ITEMS.register(ID_RED_SILICON_COMP, createSimpleItemSupplier());
        GLOW_SILICON_COMP_ITEM = ITEMS.register(ID_GLOW_SILICON_COMP, createSimpleItemSupplier());
        ELECTROTINE_SILICON_COMP_ITEM = ITEMS.register(ID_ELECTROTINE_SILICON_COMP, createSimpleItemSupplier());
        INFUSED_SILICON_ITEM = ITEMS.register(ID_INFUSED_SILICON, createSimpleItemSupplier());
        ENERGIZED_SILICON_ITEM = ITEMS.register(ID_ENERGIZED_SILICON, createSimpleItemSupplier());
        ELECTROTINE_SILICON_ITEM = ITEMS.register(ID_ELECTROTINE_SILICON, createSimpleItemSupplier());
        COPPER_COIL_ITEM = ITEMS.register(ID_COPPER_COIL, createSimpleItemSupplier());
        IRON_COIL_ITEM = ITEMS.register(ID_IRON_COIL, createSimpleItemSupplier());
        GOLD_COIL_ITEM = ITEMS.register(ID_GOLD_COIL, createSimpleItemSupplier());
        MOTOR_ITEM = ITEMS.register(ID_MOTOR, createSimpleItemSupplier());
        WOVEN_CLOTH_ITEM = ITEMS.register(ID_WOVEN_CLOTH, createSimpleItemSupplier());
        SAIL_ITEM = ITEMS.register(ID_SAIL, createSimpleItemSupplier());

        // Illumars
        WHITE_ILLUMAR_ITEM = ITEMS.register(ID_WHITE_ILLUMAR, createSimpleItemSupplier());
        ORANGE_ILLUMAR_ITEM = ITEMS.register(ID_ORANGE_ILLUMAR, createSimpleItemSupplier());
        MAGENTA_ILLUMAR_ITEM = ITEMS.register(ID_MAGENTA_ILLUMAR, createSimpleItemSupplier());
        LIGHT_BLUE_ILLUMAR_ITEM = ITEMS.register(ID_LIGHT_BLUE_ILLUMAR, createSimpleItemSupplier());
        YELLOW_ILLUMAR_ITEM = ITEMS.register(ID_YELLOW_ILLUMAR, createSimpleItemSupplier());
        LIME_ILLUMAR_ITEM = ITEMS.register(ID_LIME_ILLUMAR, createSimpleItemSupplier());
        PINK_ILLUMAR_ITEM = ITEMS.register(ID_PINK_ILLUMAR, createSimpleItemSupplier());
        GRAY_ILLUMAR_ITEM = ITEMS.register(ID_GRAY_ILLUMAR, createSimpleItemSupplier());
        LIGHT_GRAY_ILLUMAR_ITEM = ITEMS.register(ID_LIGHT_GRAY_ILLUMAR, createSimpleItemSupplier());
        CYAN_ILLUMAR_ITEM = ITEMS.register(ID_CYAN_ILLUMAR, createSimpleItemSupplier());
        PURPLE_ILLUMAR_ITEM = ITEMS.register(ID_PURPLE_ILLUMAR, createSimpleItemSupplier());
        BLUE_ILLUMAR_ITEM = ITEMS.register(ID_BLUE_ILLUMAR, createSimpleItemSupplier());
        BROWN_ILLUMAR_ITEM = ITEMS.register(ID_BROWN_ILLUMAR, createSimpleItemSupplier());
        GREEN_ILLUMAR_ITEM = ITEMS.register(ID_GREEN_ILLUMAR, createSimpleItemSupplier());
        RED_ILLUMAR_ITEM = ITEMS.register(ID_RED_ILLUMAR, createSimpleItemSupplier());
        BLACK_ILLUMAR_ITEM = ITEMS.register(ID_BLACK_ILLUMAR, createSimpleItemSupplier());

        // Tools
        DRAW_PLATE_ITEM = ITEMS.register(ID_DRAW_PLATE, DrawPlateItem::new);
        SCREWDRIVER_ITEM = ITEMS.register(ID_SCREWDRIVER, ScrewdriverItem::new);
        MULTIMETER_ITEM = ITEMS.register(ID_MULTIMETER, MultimeterItem::new);
    }

    private static Supplier<Item> createSimpleItemSupplier() {
        return () -> new Item(new Item.Properties());
    }

    //region Utilities
    public static Item getIllumarByIndex(int index) {
        return switch (index) {
            case  0 -> WHITE_ILLUMAR_ITEM.get();
            case  1 -> ORANGE_ILLUMAR_ITEM.get();
            case  2 -> MAGENTA_ILLUMAR_ITEM.get();
            case  3 -> LIGHT_BLUE_ILLUMAR_ITEM.get();
            case  4 -> YELLOW_ILLUMAR_ITEM.get();
            case  5 -> LIME_ILLUMAR_ITEM.get();
            case  6 -> PINK_ILLUMAR_ITEM.get();
            case  7 -> GRAY_ILLUMAR_ITEM.get();
            case  8 -> LIGHT_GRAY_ILLUMAR_ITEM.get();
            case  9 -> CYAN_ILLUMAR_ITEM.get();
            case 10 -> PURPLE_ILLUMAR_ITEM.get();
            case 11 -> BLUE_ILLUMAR_ITEM.get();
            case 12 -> BROWN_ILLUMAR_ITEM.get();
            case 13 -> GREEN_ILLUMAR_ITEM.get();
            case 14 -> RED_ILLUMAR_ITEM.get();
            case 15 -> BLACK_ILLUMAR_ITEM.get();
            default -> throw new IllegalArgumentException("Unexpected value: " + index);
        };
    }
    //endregion
}
