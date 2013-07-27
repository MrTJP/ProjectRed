package mrtjp.projectred.utils;

public enum PRColors {
	WHITE("White", 1.0F, 1.0F, 1.0F),
	ORANGE("Orange", 0.95F, 0.7F, 0.2F),
	MAGENTA("Magenta", 0.9F, 0.5F, 0.85F),
	LIGHT_BLUE("Light Blue", 0.6F, 0.7F, 0.95F),
	YELLOW("Yellow", 0.9F, 0.9F, 0.2F),
	LIME("Lime", 0.5F, 0.8F, 0.1F),
	PINK("Pink", 0.95F, 0.7F, 0.8F),
	GREY("Grey", 0.3F, 0.3F, 0.3F),
	LIGHT_GREY("Light Grey", 0.6F, 0.6F, 0.6F),
	CYAN("Cyan", 0.3F, 0.6F, 0.7F),
	PURPLE("Purple", 0.7F, 0.4F, 0.9F),
	BLUE("Blue", 0.2F, 0.4F, 0.8F),
	BROWN("Brown", 0.5F, 0.4F, 0.3F),
	GREEN("Green", 0.2F, 0.8F, 0.2F),
	RED("Red", 0.8F, 0.3F, 0.3F),
	BLACK("Black", 0.1F, 0.1F, 0.1F), 
	
	;

	public static final PRColors[] VALID_COLORS = { WHITE, ORANGE, MAGENTA, LIGHT_BLUE, YELLOW, LIME, PINK, GREY, LIGHT_GREY, CYAN, PURPLE, BLUE, BROWN, GREEN, RED, BLACK };
	private static final String[] dyeDictionary = { "dyeBlack", "dyeRed", "dyeGreen", "dyeBrown", "dyeBlue", "dyePurple", "dyeCyan", "dyeLightGray", "dyeGray", "dyePink", "dyeLime", "dyeYellow", "dyeLightBlue", "dyeMagenta", "dyeOrange", "dyeWhite" };

	private PRColors(String name, float r, float g, float b) {
		this.r = r;
		this.g = g;
		this.b = b;
		this.name = name;
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

	public static PRColors get(int i) {
		if (i > VALID_COLORS.length - 1) {
			return WHITE;
		}
		return VALID_COLORS[i];
	}
}
