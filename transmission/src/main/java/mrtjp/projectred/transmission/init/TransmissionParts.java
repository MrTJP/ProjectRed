package mrtjp.projectred.transmission.init;

import mrtjp.projectred.transmission.WireType;

import static mrtjp.projectred.ProjectRedTransmission.ITEMS;
import static mrtjp.projectred.ProjectRedTransmission.PARTS;

public class TransmissionParts {

    /* Wires */

    public static final String ID_RED_ALLOY = "red_alloy_wire";

    public static final String ID_INSULATED_WHITE      = "white_insulated_wire";
    public static final String ID_INSULATED_ORANGE     = "orange_insulated_wire";
    public static final String ID_INSULATED_MAGENTA    = "magenta_insulated_wire";
    public static final String ID_INSULATED_LIGHT_BLUE = "light_blue_insulated_wire";
    public static final String ID_INSULATED_YELLOW     = "yellow_insulated_wire";
    public static final String ID_INSULATED_LIME       = "lime_insulated_wire";
    public static final String ID_INSULATED_PINK       = "pink_insulated_wire";
    public static final String ID_INSULATED_GRAY       = "gray_insulated_wire";
    public static final String ID_INSULATED_LIGHT_GRAY = "light_gray_insulated_wire";
    public static final String ID_INSULATED_CYAN       = "cyan_insulated_wire";
    public static final String ID_INSULATED_PURPLE     = "purple_insulated_wire";
    public static final String ID_INSULATED_BLUE       = "blue_insulated_wire";
    public static final String ID_INSULATED_BROWN      = "brown_insulated_wire";
    public static final String ID_INSULATED_GREEN      = "green_insulated_wire";
    public static final String ID_INSULATED_RED        = "red_insulated_wire";
    public static final String ID_INSULATED_BLACK      = "black_insulated_wire";

    public static final String ID_BUNDLED_NEUTRAL    = "neutral_bundled_wire";
    public static final String ID_BUNDLED_WHITE      = "white_bundled_wire";
    public static final String ID_BUNDLED_ORANGE     = "orange_bundled_wire";
    public static final String ID_BUNDLED_MAGENTA    = "magenta_bundled_wire";
    public static final String ID_BUNDLED_LIGHT_BLUE = "light_blue_bundled_wire";
    public static final String ID_BUNDLED_YELLOW     = "yellow_bundled_wire";
    public static final String ID_BUNDLED_LIME       = "lime_bundled_wire";
    public static final String ID_BUNDLED_PINK       = "pink_bundled_wire";
    public static final String ID_BUNDLED_GRAY       = "gray_bundled_wire";
    public static final String ID_BUNDLED_LIGHT_GRAY = "light_gray_bundled_wire";
    public static final String ID_BUNDLED_CYAN       = "cyan_bundled_wire";
    public static final String ID_BUNDLED_PURPLE     = "purple_bundled_wire";
    public static final String ID_BUNDLED_BLUE       = "blue_bundled_wire";
    public static final String ID_BUNDLED_BROWN      = "brown_bundled_wire";
    public static final String ID_BUNDLED_GREEN      = "green_bundled_wire";
    public static final String ID_BUNDLED_RED        = "red_bundled_wire";
    public static final String ID_BUNDLED_BLACK      = "black_bundled_wire";

    public static final String ID_POWER_LOWLOAD      = "low_load_power_wire";

    /* Framed wires */

    public static final String ID_FRAMED_RED_ALLOY = "framed_red_alloy_wire";

    public static final String ID_FRAMED_INSULATED_WHITE      = "white_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_ORANGE     = "orange_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_MAGENTA    = "magenta_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_LIGHT_BLUE = "light_blue_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_YELLOW     = "yellow_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_LIME       = "lime_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_PINK       = "pink_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_GRAY       = "gray_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_LIGHT_GRAY = "light_gray_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_CYAN       = "cyan_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_PURPLE     = "purple_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_BLUE       = "blue_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_BROWN      = "brown_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_GREEN      = "green_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_RED        = "red_framed_insulated_wire";
    public static final String ID_FRAMED_INSULATED_BLACK      = "black_framed_insulated_wire";

    public static final String ID_FRAMED_BUNDLED_NEUTRAL    = "neutral_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_WHITE      = "white_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_ORANGE     = "orange_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_MAGENTA    = "magenta_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_LIGHT_BLUE = "light_blue_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_YELLOW     = "yellow_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_LIME       = "lime_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_PINK       = "pink_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_GRAY       = "gray_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_LIGHT_GRAY = "light_gray_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_CYAN       = "cyan_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_PURPLE     = "purple_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_BLUE       = "blue_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_BROWN      = "brown_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_GREEN      = "green_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_RED        = "red_framed_bundled_wire";
    public static final String ID_FRAMED_BUNDLED_BLACK      = "black_framed_bundled_wire";

    public static final String ID_FRAMED_POWER_LOWLOAD      = "low_load_framed_power_wire";

    public static void register() {

        // Register wires
        for (WireType type : WireType.values()) {
            type.registerParts(PARTS, ITEMS);
        }
    }
}
