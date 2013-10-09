package mrtjp.projectred.core;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import codechicken.lib.colour.Colour;
import codechicken.lib.colour.ColourRGBA;

public enum PRColors {
    WHITE(0xFFFFFF),
    ORANGE(0xC06300),
    MAGENTA(0xB51AB5),
    LIGHT_BLUE(0x6F84F1),
    YELLOW(0xBFBF00),
    LIME(0x6BF100),
    PINK(0xF14675),
    GREY(0x535353),
    LIGHT_GREY(0x939393),
    CYAN(0x008787),
    PURPLE(0x5E00C0),
    BLUE(0x1313C0),
    BROWN(0x4F2700),
    GREEN(0x088700),
    RED(0xA20F06),
    BLACK(0x1F1F1F);

    public static final PRColors[] VALID_COLORS = values();
    private static final String[] dyeDictionary = { "dyeBlack", "dyeRed", "dyeGreen", "dyeBrown", "dyeBlue", "dyePurple", "dyeCyan", "dyeLightGray", "dyeGray", "dyePink", "dyeLime", "dyeYellow", "dyeLightBlue", "dyeMagenta", "dyeOrange", "dyeWhite" };

    private PRColors(int rgb) {
        name = name().substring(0, 1)+name().substring(1).toLowerCase();
        this.rgb = rgb;
        c = new ColourRGBA(rgb<<8|0xFF);
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
    public final Colour c;
    public final int rgb;

    public static PRColors get(int i) {
        if (i > 15)
            return BLACK;
        if (i < 0)
            return WHITE;
        return VALID_COLORS[i];
    }
    
    public ItemStack getDye() {
        return new ItemStack(Item.dyePowder, 1, dyeId());
    }
}
