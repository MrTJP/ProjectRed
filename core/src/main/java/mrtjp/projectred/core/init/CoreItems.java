package mrtjp.projectred.core.init;

import mrtjp.projectred.core.item.DrawPlateItem;
import mrtjp.projectred.core.item.MultimeterItem;
import mrtjp.projectred.core.item.ScrewdriverItem;
import net.minecraft.world.item.Item;

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
    public static Supplier<Item> RED_ALLOY_INGOT_ITEM;
    public static Supplier<Item> ELECTROTINE_ALLOY_INGOT_ITEM;
    public static Supplier<Item> ELECTROTINE_DUST_ITEM;
    public static Supplier<Item> RUBY_ITEM;
    public static Supplier<Item> SAPPHIRE_ITEM;
    public static Supplier<Item> PERIDOT_ITEM;

    // Recipe ingredients
    public static Supplier<Item> PLATE_ITEM;
    public static Supplier<Item> CONDUCTIVE_PLATE_ITEM;
    public static Supplier<Item> WIRED_PLATE_ITEM;
    public static Supplier<Item> BUNDLED_PLATE_ITEM;
    public static Supplier<Item> PLATFORMED_PLATE_ITEM;
    public static Supplier<Item> ANODE_ITEM;
    public static Supplier<Item> CATHODE_ITEM;
    public static Supplier<Item> POINTER_ITEM;
    public static Supplier<Item> SILICON_CHIP_ITEM;
    public static Supplier<Item> ENERGIZED_SILICON_CHIP_ITEM;
    public static Supplier<Item> SAND_COAL_COMP_ITEM;
    public static Supplier<Item> RED_IRON_COMP_ITEM;
    public static Supplier<Item> ELECTROTINE_IRON_COMP_ITEM;
    public static Supplier<Item> SILICON_BOULE_ITEM;
    public static Supplier<Item> SILICON_ITEM;
    public static Supplier<Item> RED_SILICON_COMP_ITEM;
    public static Supplier<Item> GLOW_SILICON_COMP_ITEM;
    public static Supplier<Item> ELECTROTINE_SILICON_COMP_ITEM;
    public static Supplier<Item> INFUSED_SILICON_ITEM;
    public static Supplier<Item> ENERGIZED_SILICON_ITEM;
    public static Supplier<Item> ELECTROTINE_SILICON_ITEM;
    public static Supplier<Item> COPPER_COIL_ITEM;
    public static Supplier<Item> IRON_COIL_ITEM;
    public static Supplier<Item> GOLD_COIL_ITEM;
    public static Supplier<Item> MOTOR_ITEM;
    public static Supplier<Item> WOVEN_CLOTH_ITEM;
    public static Supplier<Item> SAIL_ITEM;

    // Illumars
    public static Supplier<Item> WHITE_ILLUMAR_ITEM;
    public static Supplier<Item> ORANGE_ILLUMAR_ITEM;
    public static Supplier<Item> MAGENTA_ILLUMAR_ITEM;
    public static Supplier<Item> LIGHT_BLUE_ILLUMAR_ITEM;
    public static Supplier<Item> YELLOW_ILLUMAR_ITEM;
    public static Supplier<Item> LIME_ILLUMAR_ITEM;
    public static Supplier<Item> PINK_ILLUMAR_ITEM;
    public static Supplier<Item> GRAY_ILLUMAR_ITEM;
    public static Supplier<Item> LIGHT_GRAY_ILLUMAR_ITEM;
    public static Supplier<Item> CYAN_ILLUMAR_ITEM;
    public static Supplier<Item> PURPLE_ILLUMAR_ITEM;
    public static Supplier<Item> BLUE_ILLUMAR_ITEM;
    public static Supplier<Item> BROWN_ILLUMAR_ITEM;
    public static Supplier<Item> GREEN_ILLUMAR_ITEM;
    public static Supplier<Item> RED_ILLUMAR_ITEM;
    public static Supplier<Item> BLACK_ILLUMAR_ITEM;

    // Tools
    public static Supplier<Item> DRAW_PLATE_ITEM;
    public static Supplier<Item> SCREWDRIVER_ITEM;
    public static Supplier<Item> MULTIMETER_ITEM;

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
