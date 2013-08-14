package mrtjp.projectred.transmission;

import java.util.Map;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.InvertX;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.oredict.OreDictionary;
import codechicken.lib.render.CCModel;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public enum EnumWire {

    RED_ALLOY("Red alloy wire", "pr_redwire", 0, 0xC80000, "redalloy"),
    
    INSULATED_0("White insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/whiteoff", "insulated/whiteon"),
    INSULATED_1("Orange insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/orangeoff", "insulated/orangeon"),
    INSULATED_2("Magenta insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/magentaoff", "insulated/magentaon"),
    INSULATED_3("Light blue insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/lightblueoff", "insulated/lightblueon"),
    INSULATED_4("Yellow insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/yellowoff", "insulated/yellowon"),
    INSULATED_5("Lime insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/limeoff", "insulated/limeon"),
    INSULATED_6("Pink insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/pinkoff", "insulated/pinkon"),
    INSULATED_7("Grey insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/greyoff", "insulated/greyon"),
    INSULATED_8("Light grey insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/lightgreyoff", "insulated/lightgreyon"),
    INSULATED_9("Cyan insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/cyanoff", "insulated/cyanon"),
    INSULATED_10("Purple insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/purpleoff", "insulated/purpleon"),
    INSULATED_11("Blue insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/blueoff", "insulated/blueon"),
    INSULATED_12("Brown insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/brownoff", "insulated/brownon"),
    INSULATED_13("Green insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/greenoff", "insulated/greenon"),
    INSULATED_14("Red insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/redoff", "insulated/redon"),
    INSULATED_15("Black insulated wire", "pr_insulated", 1, 0xFFFFFF, "insulated/blackoff", "insulated/blackon"),

    BUNDLED_N("Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/neutral"),
    BUNDLED_0("White Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/white"),
    BUNDLED_1("Orange Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/orange"),
    BUNDLED_2("Magenta Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/magenta"),
    BUNDLED_3("Light Blue Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/lightblue"),
    BUNDLED_4("Yellow Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/yellow"),
    BUNDLED_5("Lime Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/lime"),
    BUNDLED_6("Pink Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/pink"),
    BUNDLED_7("Grey Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/grey"),
    BUNDLED_8("Light Grey Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/lightgrey"),
    BUNDLED_9("Cyan Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/cyan"),
    BUNDLED_10("Purple Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/purple"),
    BUNDLED_11("Blue Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/blue"),
    BUNDLED_12("Brown Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/brown"),
    BUNDLED_13("Green Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/green"),
    BUNDLED_14("Red Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/red"),
    BUNDLED_15("Black Bundled cable", "pr_bundled", 2, 0xFFFFFF, "bundled/black");

    public static EnumWire[] VALID_WIRE = values();
    public static EnumWire[] INSULATED_WIRE = { INSULATED_0, INSULATED_1, INSULATED_2, INSULATED_3, INSULATED_4, INSULATED_5, INSULATED_6, INSULATED_7, INSULATED_8, INSULATED_9, INSULATED_10, INSULATED_11, INSULATED_12, INSULATED_13, INSULATED_14, INSULATED_15, };
    public static EnumWire[] BUNDLED_WIRE = { BUNDLED_N, BUNDLED_0, BUNDLED_1, BUNDLED_2, BUNDLED_3, BUNDLED_4, BUNDLED_5, BUNDLED_6, BUNDLED_7, BUNDLED_8, BUNDLED_9, BUNDLED_10, BUNDLED_11, BUNDLED_12, BUNDLED_13, BUNDLED_14, BUNDLED_15 };

    public final String name;
    public final String wireType;
    public final Class<? extends WirePart> jacketedClass;
    public final int thickness;
    public final int itemColour;
    
    // Rendering info
    public Icon[] wireSprites;
    public final String[] wireSpritePaths;
    public final String jacketModelPath;
    public Map<String, CCModel> jacketMap;
    public int meta = this.ordinal();

    public static final String oreDictDefinition = "projredWire";
    public static final String oreDictDefinitionInsulated = "projredInsulatedWire";
    public static final String oreDictDefinitionJacketed = "projredJacketedWire";
    public static final String oreDictDefinitionBundled = "projredBundledCable";

    private EnumWire(String name, String wireType, Class<? extends WirePart> jacketedClazz, 
        int thickness, int itemColour, String objPathJacket, String... textures) {
        this.name = name;
        this.wireType = wireType;
        this.jacketedClass = jacketedClazz;
        this.thickness = thickness;
        this.itemColour = itemColour;
        jacketModelPath = objPathJacket;
        wireSpritePaths = textures;
        wireSprites = new Icon[textures.length];
    }
    private EnumWire(String name, String wireType, int thickness, int itemColour, String... textures) {
        this(name, wireType, null, thickness, itemColour, null, textures);
    }

    public boolean hasJacketedForm() {
        return jacketedClass != null;
    }

    @SideOnly(Side.CLIENT)
    public void loadTextures(IconRegister reg) {
        if (wireSpritePaths.length > 0) {
            wireSprites = new Icon[wireSpritePaths.length];
            for (int i = 0; i < wireSpritePaths.length; i++)
                wireSprites[i] = reg.registerIcon("projectred:wires/" + wireSpritePaths[i]);
        }
        if(hasJacketedForm()) {
            jacketMap = CCModel.parseObjModels(new ResourceLocation("projectred", "/textures/obj/wiring/" + jacketModelPath), 7, new InvertX());
            for (CCModel m : jacketMap.values())
                m.shrinkUVs(0.0005);
        }
    }

    public ItemStack getItemStack() {
        return getItemStack(1);
    }

    public ItemStack getItemStack(int i) {
        return new ItemStack(ProjectRed.itemPartWire, i, meta);
    }

    public ItemStack getJacketedItemStack() {
        return getJacketedItemStack(1);
    }

    public ItemStack getJacketedItemStack(int i) {
        if (!this.hasJacketedForm())
            return null;
        
        return new ItemStack(ProjectRed.itemPartJacketedWire, i, meta);
    }

    public static void initOreDictDefinitions() {
        for (EnumWire w : EnumWire.VALID_WIRE) {
            OreDictionary.registerOre(oreDictDefinition, w.getItemStack());
            if (w.hasJacketedForm())
                OreDictionary.registerOre(oreDictDefinitionJacketed, w.getJacketedItemStack());
        }
        for (EnumWire w : EnumWire.INSULATED_WIRE)
            OreDictionary.registerOre(oreDictDefinitionInsulated, w.getItemStack());
        for (EnumWire w : EnumWire.BUNDLED_WIRE)
            OreDictionary.registerOre(oreDictDefinitionBundled, w.getItemStack());
    }
}
