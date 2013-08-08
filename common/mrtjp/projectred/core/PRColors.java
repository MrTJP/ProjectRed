package mrtjp.projectred.core;

public enum PRColors {
    WHITE("White", 1.0F, 1.0F, 1.0F, 0xFFFFFF),
    ORANGE("Orange", 0.95F, 0.7F, 0.2F, 0xFFA100),
    MAGENTA("Magenta", 0.9F, 0.5F, 0.85F, 0xFF00FF),
    LIGHT_BLUE("Light Blue", 0.6F, 0.7F, 0.95F, 0xAEAEFF),
    YELLOW("Yellow", 0.9F, 0.9F, 0.2F, 0xFFFF00),
    LIME("Lime", 0.5F, 0.8F, 0.1F, 0xA1FF63),
    PINK("Pink", 0.95F, 0.7F, 0.8F, 0xFFB9B9),
    GREY("Grey", 0.3F, 0.3F, 0.3F, 0x9D9D9D),
    LIGHT_GREY("Light Grey", 0.6F, 0.6F, 0.6F, 0xCBCBCB),
    CYAN("Cyan", 0.3F, 0.6F, 0.7F, 0x00FFFF),
    PURPLE("Purple", 0.7F, 0.4F, 0.9F, 0xAE00FF),
    BLUE("Blue", 0.2F, 0.4F, 0.8F, 0x0000FF),
    BROWN("Brown", 0.5F, 0.4F, 0.3F, 0xA55A00),
    GREEN("Green", 0.2F, 0.8F, 0.2F, 0x00A600),
    RED("Red", 0.8F, 0.3F, 0.3F, 0xFF0000),
    BLACK("Black", 0.1F, 0.1F, 0.1F, 0x3B3B3B),

    ;

    public static final PRColors[] VALID_COLORS = values();
    private static final String[] dyeDictionary = { "dyeBlack", "dyeRed", "dyeGreen", "dyeBrown", "dyeBlue", "dyePurple", "dyeCyan", "dyeLightGray", "dyeGray", "dyePink", "dyeLime", "dyeYellow", "dyeLightBlue", "dyeMagenta", "dyeOrange", "dyeWhite" };

    private PRColors(String name, float r, float g, float b, int hexcode) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.name = name;
        this.hex = hexcode;
    }

    public int dyeId() {
        return 15 - ordinal();
    }

    public int woolId() {
        return ordinal();
    }

    public String getOreDict() {
        return dyeDictionary[dyeId()];
    }

    public final String name;
    public final float r, g, b;
    public final int hex;

    public static PRColors get(int i) {
        if (i > VALID_COLORS.length - 1) {
            return WHITE;
        }
        return VALID_COLORS[i];
    }
}
