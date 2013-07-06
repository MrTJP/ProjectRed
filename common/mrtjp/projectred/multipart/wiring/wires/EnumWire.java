package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.utils.BasicRenderUtils;
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
	RED_ALLOY("Red alloy wire", "wire-red-alloy", "", 2, 2, TilePlainRedAlloy.class, 0x800000),
	INSULATED_0("White insulated wire", "wire-insulated>", "0", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_1("Orange insulated wire", "wire-insulated>", "1", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_2("Magenta insulated wire", "wire-insulated>", "2", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_3("Light blue insulated wire", "wire-insulated>", "3", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_4("Yellow insulated wire", "wire-insulated>", "4", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_5("Lime insulated wire", "wire-insulated>", "5", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_6("Pink insulated wire", "wire-insulated>", "6", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_7("Grey insulated wire", "wire-insulated>", "7", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_8("Light grey insulated wire", "wire-insulated>", "8", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_9("Cyan insulated wire", "wire-insulated>", "9", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_A("Purple insulated wire", "wire-insulated>", "10", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_B("Blue insulated wire", "wire-insulated>", "11", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_C("Brown insulated wire", "wire-insulated>", "12", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_D("Green insulated wire", "wire-insulated>", "13", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_E("Red insulated wire", "wire-insulated>", "14", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	INSULATED_F("Black insulated wire", "wire-insulated>", "15", 3, 4, TileInsulatedRedAlloy.class, 0xFFFFFF),
	BUNDLED("Bundled cable", "wire-bundled>", "", 4, 6, TileBundled.class, 0xFFFFFF);

	public static final int PLAIN_RED_ALLOY_META = 0;
	public static final int INSULATED_RED_ALLOY_META = 1;
	public static final int BUNDLED_META = 2;

	public static ImmutableBiMap<Class<? extends TileWire>, Integer> CLASS_TO_META = ImmutableBiMap.<Class<? extends TileWire>, Integer> builder().put(TilePlainRedAlloy.class, PLAIN_RED_ALLOY_META).put(TileInsulatedRedAlloy.class, INSULATED_RED_ALLOY_META).put(TileBundled.class, BUNDLED_META).build();

	public static ImmutableBiMap<Integer, Class<? extends TileWire>> META_TO_CLASS = CLASS_TO_META.inverse();

	public static EnumWire[] INSULATED_WIRE = { INSULATED_0, INSULATED_1, INSULATED_2, INSULATED_3, INSULATED_4, INSULATED_5, INSULATED_6, INSULATED_7, INSULATED_8, INSULATED_9, INSULATED_A, INSULATED_B, INSULATED_C, INSULATED_D, INSULATED_E, INSULATED_F, };
	public static EnumWire[] VALID_WIRE = { RED_ALLOY, INSULATED_0, INSULATED_1, INSULATED_2, INSULATED_3, INSULATED_4, INSULATED_5, INSULATED_6, INSULATED_7, INSULATED_8, INSULATED_9, INSULATED_A, INSULATED_B, INSULATED_C, INSULATED_D, INSULATED_E, INSULATED_F, BUNDLED };

	public final String name;
	public final String textureName, texNameSuffix;
	public final double thickness, width;
	public final Class<? extends TileWire> teclass;
	public final int itemColour;
	public int meta = this.ordinal();	
	public static final String oreDictDefinition = "projredWire";
	public static final String oreDictDefinitionInsulated = "projredInsulatedWire";
	public static final String oreDictDefinitionJacketed = "projredJacketedWire";

	public boolean hasJacketedForm() {
		return true;
	}

	@SideOnly(Side.CLIENT)
	public Icon texture_cross, texture_straight_x, texture_straight_z,
			texture_none, texture_tee_nz, texture_tee_pz, texture_tee_nx,
			texture_tee_px, texture_corner_nn, texture_corner_np,
			texture_corner_pn, texture_corner_pp, texture_end_nx,
			texture_end_px, texture_end_nz, texture_end_pz, texture_jacketed,
			texture_jacketed_end, texture_jacketed_cross;

	private EnumWire(String name, String texName, String texSuffix, int thicknessPixels, int widthPixels, Class<? extends TileWire> teclass, int itemColour) {
		this.name = name;
		this.textureName = "projectred:" + texName;
		this.texNameSuffix = texSuffix;
		this.thickness = thicknessPixels / 16.0;
		this.width = widthPixels / 16.0;
		this.teclass = teclass;
		this.itemColour = itemColour;
	}

	public static final EnumWire[] VALUES = values();

	@SideOnly(Side.CLIENT)
	public void loadTextures(IconRegister reg, String base, String suffix) {
		if (!base.endsWith(">")) {
			Icon i = reg.registerIcon(base);
			texture_cross = i;
			texture_straight_x = i;
			texture_straight_z = i;
			texture_tee_nz = i;
			texture_tee_pz = i;
			texture_tee_nx = i;
			texture_tee_px = i;
			texture_corner_nn = i;
			texture_corner_np = i;
			texture_corner_pn = i;
			texture_corner_pp = i;
			texture_none = i;
			texture_end_px = i;
			texture_end_nx = i;
			texture_end_pz = i;
			texture_end_nz = i;

		} else {
			texture_cross = BasicRenderUtils.loadIcon(reg, base + "cross" + suffix);
			texture_straight_x = BasicRenderUtils.loadIcon(reg, base + "straight-x" + suffix);
			texture_straight_z = BasicRenderUtils.loadIcon(reg, base + "straight-z" + suffix);
			texture_tee_nz = BasicRenderUtils.loadIcon(reg, base + "tee-nz" + suffix);
			texture_tee_pz = BasicRenderUtils.loadIcon(reg, base + "tee-pz" + suffix);
			texture_tee_nx = BasicRenderUtils.loadIcon(reg, base + "tee-nx" + suffix);
			texture_tee_px = BasicRenderUtils.loadIcon(reg, base + "tee-px" + suffix);
			texture_corner_nn = BasicRenderUtils.loadIcon(reg, base + "corner-nn" + suffix);
			texture_corner_np = BasicRenderUtils.loadIcon(reg, base + "corner-np" + suffix);
			texture_corner_pn = BasicRenderUtils.loadIcon(reg, base + "corner-pn" + suffix);
			texture_corner_pp = BasicRenderUtils.loadIcon(reg, base + "corner-pp" + suffix);
			texture_none = BasicRenderUtils.loadIcon(reg, base + "none" + suffix);
			texture_end_px = BasicRenderUtils.loadIcon(reg, base + "end-px" + suffix);
			texture_end_nx = BasicRenderUtils.loadIcon(reg, base + "end-nx" + suffix);
			texture_end_pz = BasicRenderUtils.loadIcon(reg, base + "end-pz" + suffix);
			texture_end_nz = BasicRenderUtils.loadIcon(reg, base + "end-nz" + suffix);
		}

		if (hasJacketedForm()) {
			String jacketedBase;
			if (base.endsWith(">")) {
				jacketedBase = base + "jacketed";
			} else {
				jacketedBase = base + "-j";
			}
			texture_jacketed = BasicRenderUtils.loadIcon(reg, jacketedBase + suffix);
			texture_jacketed_end = BasicRenderUtils.loadIcon(reg, jacketedBase + "-end" + suffix);
			texture_jacketed_cross = BasicRenderUtils.loadIcon(reg, jacketedBase + "-cross" + suffix);
		}
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
		for (EnumWire w: EnumWire.INSULATED_WIRE) {
			OreDictionary.registerOre(oreDictDefinitionInsulated, w.getItemStack());
		}
	}
}
