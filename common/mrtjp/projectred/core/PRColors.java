package mrtjp.projectred.core;

public enum PRColors {
    WHITE("White", 1.0F, 1.0F, 1.0F, 16777215),
    ORANGE("Orange", 0.95F, 0.7F, 0.2F, 12608256),
    MAGENTA("Magenta", 0.9F, 0.5F, 0.85F, 11868853),
    LIGHT_BLUE("Light Blue", 0.6F, 0.7F, 0.95F, 7308529),
    YELLOW("Yellow", 0.9F, 0.9F, 0.2F, 12566272),
    LIME("Lime", 0.5F, 0.8F, 0.1F, 7074048),
    PINK("Pink", 0.95F, 0.7F, 0.8F, 15812213),
    GREY("Grey", 0.3F, 0.3F, 0.3F, 5460819),
    LIGHT_GREY("Light Grey", 0.6F, 0.6F, 0.6F, 9671571),
    CYAN("Cyan", 0.3F, 0.6F, 0.7F, 34695),
    PURPLE("Purple", 0.7F, 0.4F, 0.9F, 6160576),
    BLUE("Blue", 0.2F, 0.4F, 0.8F, 1250240),
    BROWN("Brown", 0.5F, 0.4F, 0.3F, 5187328),
    GREEN("Green", 0.2F, 0.8F, 0.2F, 558848),
    RED("Red", 0.8F, 0.3F, 0.3F, 10620678),
    BLACK("Black", 0.1F, 0.1F, 0.1F, 2039583),

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
        if (i > VALID_COLORS.length - 1)
            return WHITE;
        return VALID_COLORS[i];
    }
}
