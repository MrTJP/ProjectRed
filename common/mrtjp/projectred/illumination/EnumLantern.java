package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.oredict.OreDictionary;

public enum EnumLantern {
    WHITE("White Lantern", "lanternwhite"),
    ORANGE("Orange Lantern", "lanternorange"),
    MAGENTA("Magenta Lantern", "lanternmagenta"),
    LIGHTBLUE("Light Blue Lantern", "lanternlightblue"),
    YELLOW("Yellow Lantern", "lanternyellow"),
    LIME("Lime Lantern", "lanternlime"),
    PINK("Pink Lantern", "lanternpink"),
    GREY("Grey Lantern", "lanterngrey"),
    LIGHTGREY("Light Grey Lantern", "lanternlightgrey"),
    CYAN("Cyan Lantern", "lanterncyan"),
    PURPLE("Purple Lantern", "lanternpurple"),
    BLUE("Blue Lantern", "lanternblue"),
    BROWN("Brown Lantern", "lanternbrown"),
    GREEN("Green Lantern", "lanterngreen"),
    RED("Red Lantern", "lanternred"),
    BLACK("Black Lantern", "lanternblack"),
    ;
    public final String fullName;
    public final String unlocalName;
    public static final EnumLantern[] VALID_TYPES = values();
    public int meta = this.ordinal();
    public static final String oreDictDefinition = "projredLantern";
    public Icon offIcon;
    public Icon onIcon;
    private boolean iconsLoaded;
    
    private EnumLantern(String name, String unlocal) {
        fullName = name;
        unlocalName = unlocal;
    }

    public static EnumLantern get(int i) {
        if (i < 0 || i > VALID_TYPES.length - 1) {
            return null;
        }
        return VALID_TYPES[i];
    }
    
    public void registerIcon(IconRegister reg) {
        if (iconsLoaded) {
            return;
        }
        offIcon = reg.registerIcon("projectred:" + "lanternoff/" + unlocalName + "off");
        onIcon = reg.registerIcon("projectred:" + "lanternon/" + unlocalName + "on");
        iconsLoaded = true;
    }

    public ItemStack getItemStack() {
        return new ItemStack(ProjectRedIllumination.itemPartLantern, 1, meta);
    }

    public ItemStack getInvertedItemStack() {
        return new ItemStack(ProjectRedIllumination.itemPartInvLantern, 1, meta);
    }

    public static void initOreDictDefinitions() {
        for (EnumLantern l : EnumLantern.VALID_TYPES) {
            OreDictionary.registerOre(oreDictDefinition, l.getItemStack());
            OreDictionary.registerOre(oreDictDefinition, l.getInvertedItemStack());
        }
    }
}
