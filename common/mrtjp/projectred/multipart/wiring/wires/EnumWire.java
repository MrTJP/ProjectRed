package mrtjp.projectred.multipart.wiring.wires;

import java.util.Map;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.utils.codechicken.core.render.CCModel;
import mrtjp.projectred.utils.codechicken.core.vec.InvertX;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.oredict.OreDictionary;

import com.google.common.collect.ImmutableBiMap;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

// Damage value for wire items is the ordinal of one of these, but metadata in the world
// is different, and depends on the TE class (for client sync purposes).
// Metadata mapping is in CLASS_TO_META and META_TO_CLASS.

public enum EnumWire {
	RED_ALLOY("Red alloy wire", TilePlainRedAlloy.class, 2, 2, 0x800000, "alloywire.obj", "redalloy"),
	INSULATED_0("White insulated wire", TileInsulatedRedAlloy.class, 3, 4,  "insulatedwire.obj", "insulated/whiteoff", "insulated/whiteon"),
	INSULATED_1("Orange insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/orangeoff", "insulated/orangeon"),
	INSULATED_2("Magenta insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/magentaoff", "insulated/magentaon"),
	INSULATED_3("Light blue insulated wire",TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/lightblueoff", "insulated/lightblueon"),
	INSULATED_4("Yellow insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/yellowoff", "insulated/yellowon"),
	INSULATED_5("Lime insulated wire",TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/limeoff", "insulated/limeon"),
	INSULATED_6("Pink insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/pinkoff", "insulated/pinkon"),
	INSULATED_7("Grey insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/greyoff", "insulated/greyon"),
	INSULATED_8("Light grey insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/lightgreyoff", "insulated/lightgreyon"),
	INSULATED_9("Cyan insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/cyanoff", "insulated/cyanon"),
	INSULATED_A("Purple insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/purpleoff", "insulated/purpleon"),
	INSULATED_B("Blue insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/blueoff", "insulated/blueon"),
	INSULATED_C("Brown insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/brownoff", "insulated/brownon"),
	INSULATED_D("Green insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/greenoff", "insulated/greenon"),
	INSULATED_E("Red insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/redoff", "insulated/redon"),
	INSULATED_F("Black insulated wire", TileInsulatedRedAlloy.class, 3,4,  "insulatedwire.obj", "insulated/blackoff", "insulated/blackon"),
	BUNDLED_N("Bundled cable", TileBundled.class, 4, 6, "bundledcable.obj", "bundled/neutral");

	public static final int PLAIN_RED_ALLOY_META = 0;
	public static final int INSULATED_RED_ALLOY_META = 1;
	public static final int BUNDLED_META = 2;

	public static ImmutableBiMap<Class<? extends TileWire>, Integer> CLASS_TO_META = ImmutableBiMap.<Class<? extends TileWire>, Integer> builder().put(TilePlainRedAlloy.class, PLAIN_RED_ALLOY_META).put(TileInsulatedRedAlloy.class, INSULATED_RED_ALLOY_META).put(TileBundled.class, BUNDLED_META).build();

	public static ImmutableBiMap<Integer, Class<? extends TileWire>> META_TO_CLASS = CLASS_TO_META.inverse();

	public static EnumWire[] INSULATED_WIRE = { INSULATED_0, INSULATED_1, INSULATED_2, INSULATED_3, INSULATED_4, INSULATED_5, INSULATED_6, INSULATED_7, INSULATED_8, INSULATED_9, INSULATED_A, INSULATED_B, INSULATED_C, INSULATED_D, INSULATED_E, INSULATED_F, };
	public static EnumWire[] VALID_WIRE = { RED_ALLOY, INSULATED_0, INSULATED_1, INSULATED_2, INSULATED_3, INSULATED_4, INSULATED_5, INSULATED_6, INSULATED_7, INSULATED_8, INSULATED_9, INSULATED_A, INSULATED_B, INSULATED_C, INSULATED_D, INSULATED_E, INSULATED_F, BUNDLED_N };

	public final String name;
	public final Class<? extends TileWire> teclass;
	public final int itemColour;
	public final double thickness, width;

	public static final String oreDictDefinition = "projredWire";
	public static final String oreDictDefinitionInsulated = "projredInsulatedWire";
	public static final String oreDictDefinitionJacketed = "projredJacketedWire";
	public int meta = this.ordinal();

	public boolean hasJacketedForm() {
		return true;
	}

	public Icon[] wireSprites;
	public String[] wireSpritePaths;

	public String wireModelPath;
	public Map<String, CCModel> wireMap;

	private EnumWire(String name, Class<? extends TileWire> tileClazz, int thicknessPixels, int widthPixels, int itemColour, String objPath, String... textures) {
		this.name = name;
		this.teclass = tileClazz;
		this.thickness = thicknessPixels / 16.0;
		this.width = widthPixels / 16.0;
		this.itemColour = itemColour;
		wireModelPath = objPath;
		wireSpritePaths = textures;
		wireSprites = new Icon[textures.length];
	}

	private EnumWire(String name, Class<? extends TileWire> tileClazz, int thicknessPixels, int widthPixels, String objPath, String... textures) {
		this(name, tileClazz, thicknessPixels, widthPixels, 0xFFFFFF, objPath, textures);
	}

	public static final EnumWire[] VALUES = values();

	@SideOnly(Side.CLIENT)
	public void loadTextures(IconRegister reg) {
		if (wireSpritePaths.length > 0) {
			wireSprites = new Icon[wireSpritePaths.length];
			for (int i = 0; i < wireSpritePaths.length; i++) {
				wireSprites[i] = reg.registerIcon("projectred:/wires/" + wireSpritePaths[i]);
			}
		}
		wireMap = CCModel.parseObjModels("/mods/projectred/textures/obj/wiring/" + wireModelPath, 7, new InvertX());
	}

	public ItemStack getItemStack() {
		return new ItemStack(ProjectRed.blockWire, 1, meta);
	}

	public ItemStack getItemStack(int i) {
		return new ItemStack(ProjectRed.blockWire, i, meta);
	}

	public ItemStack getJacketedItemStack() {
		return new ItemStack(ProjectRed.blockWire, 1, meta | WireDamageValues.DMG_FLAG_JACKETED);
	}

	public ItemStack getJacketedItemStack(int i) {
		if (!this.hasJacketedForm()) {
			return null;
		}
		return new ItemStack(ProjectRed.blockWire, i, meta | WireDamageValues.DMG_FLAG_JACKETED);
	}

	public static boolean isInsulatedWire(ItemStack stack) {
		for (EnumWire w : EnumWire.INSULATED_WIRE) {
			if (stack.itemID == ProjectRed.blockWire.blockID && stack.getItemDamage() == w.meta) {
				return true;
			}
		}
		return false;
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
	}

	public static class WireDamageValues {
		// things controlling interpretation of item damage values
		public static final int DMG_FLAG_JACKETED = 16384;
		public static final int DMG_MASK_ORDINAL = 255;

		public static boolean isJacketed(int damageValue) {
			return (damageValue & DMG_FLAG_JACKETED) != 0;
		}

		public static EnumWire getType(int damageValue) {
			int ordinal = damageValue & DMG_MASK_ORDINAL;
			if (ordinal < 0 || ordinal >= EnumWire.VALUES.length) {
				return null;
			}
			return EnumWire.VALUES[ordinal];
		}
	}
}
