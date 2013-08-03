package mrtjp.projectred.transmission;

import java.util.Map;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.utils.codechicken.core.vec.InvertX;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.oredict.OreDictionary;
import codechicken.lib.render.CCModel;

import com.google.common.collect.ImmutableBiMap;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

// Damage value for wire items is the ordinal of one of these, but metadata in the world
// is different, and depends on the TE class (for client sync purposes).
// Metadata mapping is in CLASS_TO_META and META_TO_CLASS.

public enum EnumWire {
	RED_ALLOY("Red alloy wire", RedAlloyWirePart.class, 2, 2, (255 / 2 + 75) << 16, "alloywire.obj", "jacketedalloy.obj", "redalloy"),

	INSULATED_0("White insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/whiteoff", "insulated/whiteon"),
	INSULATED_1("Orange insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/orangeoff", "insulated/orangeon"),
	INSULATED_2("Magenta insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/magentaoff", "insulated/magentaon"),
	INSULATED_3("Light blue insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/lightblueoff", "insulated/lightblueon"),
	INSULATED_4("Yellow insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/yellowoff", "insulated/yellowon"),
	INSULATED_5("Lime insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/limeoff", "insulated/limeon"),
	INSULATED_6("Pink insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/pinkoff", "insulated/pinkon"),
	INSULATED_7("Grey insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/greyoff", "insulated/greyon"),
	INSULATED_8("Light grey insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/lightgreyoff", "insulated/lightgreyon"),
	INSULATED_9("Cyan insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/cyanoff", "insulated/cyanon"),
	INSULATED_10("Purple insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/purpleoff", "insulated/purpleon"),
	INSULATED_11("Blue insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/blueoff", "insulated/blueon"),
	INSULATED_12("Brown insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/brownoff", "insulated/brownon"),
	INSULATED_13("Green insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/greenoff", "insulated/greenon"),
	INSULATED_14("Red insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/redoff", "insulated/redon"),
	INSULATED_15("Black insulated wire", InsulatedRedAlloyPart.class, 3, 4, "insulatedwire.obj", "jacketedinsulated.obj", "insulated/blackoff", "insulated/blackon"),

	BUNDLED_N("Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/neutral"),
	BUNDLED_0("White Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/white"),
	BUNDLED_1("Orange Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/orange"),
	BUNDLED_2("Magenta Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/magenta"),
	BUNDLED_3("Light Blue Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/lightblue"),
	BUNDLED_4("Yellow Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/yellow"),
	BUNDLED_5("Lime Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/lime"),
	BUNDLED_6("Pink Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/pink"),
	BUNDLED_7("Grey Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/grey"),
	BUNDLED_8("Light Grey Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/lightgrey"),
	BUNDLED_9("Cyan Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/cyan"),
	BUNDLED_10("Purple Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/purple"),
	BUNDLED_11("Blue Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/blue"),
	BUNDLED_12("Brown Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/brown"),
	BUNDLED_13("Green Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/green"),
	BUNDLED_14("Red Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/red"),
	BUNDLED_15("Black Bundled cable", BundledCablePart.class, 4, 6, "bundledcable.obj", "jacketedbundled.obj", "bundled/black"), ;

	public static final int PLAIN_RED_ALLOY_META = 0;
	public static final int INSULATED_RED_ALLOY_META = 1;
	public static final int BUNDLED_META = 2;

	public static ImmutableBiMap<Class<? extends WirePart>, Integer> CLASS_TO_META = ImmutableBiMap.<Class<? extends WirePart>, Integer> builder().put(RedAlloyWirePart.class, PLAIN_RED_ALLOY_META).put(InsulatedRedAlloyPart.class, INSULATED_RED_ALLOY_META).put(BundledCablePart.class, BUNDLED_META).build();

	public static ImmutableBiMap<Integer, Class<? extends WirePart>> META_TO_CLASS = CLASS_TO_META.inverse();

	public static EnumWire[] VALID_WIRE = values();
	public static EnumWire[] INSULATED_WIRE = { INSULATED_0, INSULATED_1, INSULATED_2, INSULATED_3, INSULATED_4, INSULATED_5, INSULATED_6, INSULATED_7, INSULATED_8, INSULATED_9, INSULATED_10, INSULATED_11, INSULATED_12, INSULATED_13, INSULATED_14, INSULATED_15, };
	public static EnumWire[] BUNDLED_WIRE = { BUNDLED_N, BUNDLED_0, BUNDLED_1, BUNDLED_2, BUNDLED_3, BUNDLED_4, BUNDLED_5, BUNDLED_6, BUNDLED_7, BUNDLED_8, BUNDLED_9, BUNDLED_10, BUNDLED_11, BUNDLED_12, BUNDLED_13, BUNDLED_14, BUNDLED_15 };

	public final String name;
	public final Class<? extends WirePart> teclass;
	public final int itemColour;
	public final double thickness, width;

	// Rendering info
	public Icon[] wireSprites;
	public final String[] wireSpritePaths;
	public final String wireModelPath;
	public final String jacketModelPath;
	public Map<String, CCModel> wireMap;
	public Map<String, CCModel> jacketMap;

	public static final String oreDictDefinition = "projredWire";
	public static final String oreDictDefinitionInsulated = "projredInsulatedWire";
	public static final String oreDictDefinitionJacketed = "projredJacketedWire";
	public static final String oreDictDefinitionBundled = "projredBundledCable";

	public int meta = this.ordinal();

	private EnumWire(String name, Class<? extends WirePart> tileClazz, int thicknessPixels, int widthPixels, int itemColour, String objPathWire, String objPathJacket, String... textures) {
		this.name = name;
		this.teclass = tileClazz;
		this.thickness = thicknessPixels / 16.0;
		this.width = widthPixels / 16.0;
		this.itemColour = itemColour;
		wireModelPath = objPathWire;
		jacketModelPath = objPathJacket;
		wireSpritePaths = textures;
		wireSprites = new Icon[textures.length];
	}

	private EnumWire(String name, Class<? extends WirePart> tileClazz, int thicknessPixels, int widthPixels, String objPath, String objPathJacket, String... textures) {
		this(name, tileClazz, thicknessPixels, widthPixels, 0xFFFFFF, objPath, objPathJacket, textures);
	}

	public boolean hasJacketedForm() {
		return true;
	}

	@SideOnly(Side.CLIENT)
	public void loadTextures(IconRegister reg) {
		if (wireSpritePaths.length > 0) {
			wireSprites = new Icon[wireSpritePaths.length];
			for (int i = 0; i < wireSpritePaths.length; i++) {
				wireSprites[i] = reg.registerIcon("projectred:wires/" + wireSpritePaths[i]);
			}
		}
		wireMap = CCModel.parseObjModels(new ResourceLocation("projectred", "/textures/obj/wiring/" + wireModelPath), 7, new InvertX());
		jacketMap = CCModel.parseObjModels(new ResourceLocation("projectred", "/textures/obj/wiring/" + jacketModelPath), 7, new InvertX());
		for (CCModel m : wireMap.values()) {
			m.shrinkUVs(0.0005);
		}
		for (CCModel m : jacketMap.values()) {
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
		// TODO change this
		if (!this.hasJacketedForm()) {
			return null;
		}
		return new ItemStack(ProjectRed.itemPartWire, i, meta);
	}

	public static boolean isBundledCable(ItemStack stack) {
		for (EnumWire w : EnumWire.BUNDLED_WIRE) {
			if (stack.itemID == ProjectRed.itemPartWire.itemID && stack.getItemDamage() == w.meta) {
				return true;
			}
		}
		return false;
	}

	public static boolean isInsulatedWire(ItemStack stack) {
		for (EnumWire w : EnumWire.INSULATED_WIRE) {
			if (stack.itemID == ProjectRed.itemPartWire.itemID && stack.getItemDamage() == w.meta) {
				return true;
			}
		}
		return false;
	}

	public static EnumWire getTypeByName(String name) {
		for (EnumWire w : VALID_WIRE) {
			if (name == w.name) {
				return w;
			}
		}
		return null;
	}

	public static void initOreDictDefinitions() {
		for (EnumWire w : EnumWire.VALID_WIRE) {
			OreDictionary.registerOre(oreDictDefinition, w.getItemStack());
			if (w.hasJacketedForm()) {
				OreDictionary.registerOre(oreDictDefinitionJacketed, w.getJacketedItemStack());
			}
		}
		for (EnumWire w : EnumWire.INSULATED_WIRE) {
			OreDictionary.registerOre(oreDictDefinitionInsulated, w.getItemStack());
		}
		for (EnumWire w : EnumWire.BUNDLED_WIRE) {
			OreDictionary.registerOre(oreDictDefinitionBundled, w.getItemStack());
		}
	}
}
