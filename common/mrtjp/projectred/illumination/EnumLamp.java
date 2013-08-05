package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRed;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.oredict.OreDictionary;

public enum EnumLamp {
	WHITE("White Lamp", "lampwhite"),
	ORANGE("Orange Lamp", "lamporange"),
	MAGENTA("Magenta Lamp", "lampmagenta"),
	LIGHTBLUE("Light Blue Lamp", "lamplightblue"),
	YELLOW("Yellow Lamp", "lampyellow"),
	LIME("Lime Lamp", "lamplime"),
	PINK("Pink Lamp", "lamppink"),
	GREY("Grey Lamp", "lampgrey"),
	LIGHTGREY("Light Grey Lamp", "lamplightgrey"),
	CYAN("Cyan Lamp", "lampcyan"),
	PURPLE("Purple Lamp", "lamppurple"),
	BLUE("Blue Lamp", "lampblue"),
	BROWN("Brown Lamp", "lampbrown"),
	GREEN("Green Lamp", "lampgreen"),
	RED("Red Lamp", "lampred"),
	BLACK("Black Lamp", "lampblack"),

	;
	public final String fullName;
	public final String unlocalName;
	public static final EnumLamp[] VALID_TYPES = values();
	public int meta = this.ordinal();
	public static final String oreDictDefinition = "projredLamp";
	public Icon offIcon;
	public Icon onIcon;
	private boolean iconsInitialized = false;

	private EnumLamp(String name, String unlocal) {
		fullName = name;
		unlocalName = unlocal;
	}

	public static EnumLamp get(int i) {
		if (i > VALID_TYPES.length - 1) {
			return null;
		}
		return VALID_TYPES[i];
	}

	public void registerIcon(IconRegister reg) {
		if (!iconsInitialized) {
			onIcon = reg.registerIcon("projectred:lampon/" + unlocalName + "on");
			offIcon = reg.registerIcon("projectred:lampoff/" + unlocalName + "off");
			iconsInitialized = true;
		}
	}

	public ItemStack getItemStack() {
		return new ItemStack(ProjectRed.itemPartLamp, 1, meta);
	}

	public ItemStack getInvertedItemStack() {
		return new ItemStack(ProjectRed.itemPartInvLamp, 1, meta);
	}

	public static void initOreDictDefinitions() {
		for (EnumLamp l : EnumLamp.VALID_TYPES) {
			OreDictionary.registerOre(oreDictDefinition, l.getItemStack());
			OreDictionary.registerOre(oreDictDefinition, l.getInvertedItemStack());
		}
	}
}
