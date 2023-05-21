package mrtjp.projectred.core.init;

import mrtjp.projectred.core.item.DrawPlateItem;
import mrtjp.projectred.core.item.MultimeterItem;
import mrtjp.projectred.core.item.ScrewdriverItem;
import net.minecraft.world.item.Item;

import java.util.function.Supplier;

import static mrtjp.projectred.core.ProjectRedCore.CORE_CREATIVE_TAB;
import static mrtjp.projectred.core.ProjectRedCore.ITEMS;

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

    public static void register() {

        // Ingots/dusts/gems
        ITEMS.register(ID_RED_ALLOY_INGOT, createSimpleItemSupplier());
        ITEMS.register(ID_ELECTROTINE_ALLOY_INGOT, createSimpleItemSupplier());
        ITEMS.register(ID_ELECTROTINE_DUST, createSimpleItemSupplier());
        ITEMS.register(ID_RUBY, createSimpleItemSupplier());
        ITEMS.register(ID_SAPPHIRE, createSimpleItemSupplier());
        ITEMS.register(ID_PERIDOT, createSimpleItemSupplier());

        // Recipe ingredients
        ITEMS.register(ID_PLATE, createSimpleItemSupplier());
        ITEMS.register(ID_CONDUCTIVE_PLATE, createSimpleItemSupplier());
        ITEMS.register(ID_WIRED_PLATE, createSimpleItemSupplier());
        ITEMS.register(ID_BUNDLED_PLATE, createSimpleItemSupplier());
        ITEMS.register(ID_PLATFORMED_PLATE, createSimpleItemSupplier());
        ITEMS.register(ID_ANODE, createSimpleItemSupplier());
        ITEMS.register(ID_CATHODE, createSimpleItemSupplier());
        ITEMS.register(ID_POINTER, createSimpleItemSupplier());
        ITEMS.register(ID_SILICON_CHIP, createSimpleItemSupplier());
        ITEMS.register(ID_ENERGIZED_SILICON_CHIP, createSimpleItemSupplier());
        ITEMS.register(ID_SAND_COAL_COMP, createSimpleItemSupplier());
        ITEMS.register(ID_RED_IRON_COMP, createSimpleItemSupplier());
        ITEMS.register(ID_ELECTROTINE_IRON_COMP, createSimpleItemSupplier());
        ITEMS.register(ID_SILICON_BOULE, createSimpleItemSupplier());
        ITEMS.register(ID_SILICON, createSimpleItemSupplier());
        ITEMS.register(ID_RED_SILICON_COMP, createSimpleItemSupplier());
        ITEMS.register(ID_GLOW_SILICON_COMP, createSimpleItemSupplier());
        ITEMS.register(ID_ELECTROTINE_SILICON_COMP, createSimpleItemSupplier());
        ITEMS.register(ID_INFUSED_SILICON, createSimpleItemSupplier());
        ITEMS.register(ID_ENERGIZED_SILICON, createSimpleItemSupplier());
        ITEMS.register(ID_ELECTROTINE_SILICON, createSimpleItemSupplier());
        ITEMS.register(ID_COPPER_COIL, createSimpleItemSupplier());
        ITEMS.register(ID_IRON_COIL, createSimpleItemSupplier());
        ITEMS.register(ID_GOLD_COIL, createSimpleItemSupplier());
        ITEMS.register(ID_MOTOR, createSimpleItemSupplier());
        ITEMS.register(ID_WOVEN_CLOTH, createSimpleItemSupplier());
        ITEMS.register(ID_SAIL, createSimpleItemSupplier());
        ITEMS.register(ID_WHITE_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_ORANGE_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_MAGENTA_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_LIGHT_BLUE_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_YELLOW_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_LIME_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_PINK_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_GRAY_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_LIGHT_GRAY_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_CYAN_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_PURPLE_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_BLUE_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_BROWN_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_GREEN_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_RED_ILLUMAR, createSimpleItemSupplier());
        ITEMS.register(ID_BLACK_ILLUMAR, createSimpleItemSupplier());

        // Tools
        ITEMS.register(ID_DRAW_PLATE, DrawPlateItem::new);
        ITEMS.register(ID_SCREWDRIVER, ScrewdriverItem::new);
        ITEMS.register(ID_MULTIMETER, MultimeterItem::new);
    }

    private static Supplier<Item> createSimpleItemSupplier() {
        return () -> new Item(new Item.Properties().tab(CORE_CREATIVE_TAB));
    }
}
