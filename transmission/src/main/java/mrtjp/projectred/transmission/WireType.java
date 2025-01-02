package mrtjp.projectred.transmission;

import codechicken.lib.colour.EnumColour;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import com.google.common.collect.ImmutableList;
import mrtjp.projectred.transmission.item.CenterWirePartItem;
import mrtjp.projectred.transmission.item.FaceWirePartItem;
import mrtjp.projectred.transmission.part.*;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.neoforge.client.event.TextureAtlasStitchedEvent;
import net.neoforged.neoforge.registries.DeferredRegister;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;
import static mrtjp.projectred.transmission.init.TransmissionParts.*;

public enum WireType {

    //@formatter:off
    RED_ALLOY(ID_RED_ALLOY, false, RedAlloyWirePart::new, 0, 0xC80000, "red_alloy_wire"),

    INSULATED_WHITE     (ID_INSULATED_WHITE,      false, InsulatedRedAlloyWirePart::new, EnumColour.WHITE,      1, "white_insulated_wire_off",      "white_insulated_wire_on"),
    INSULATED_ORANGE    (ID_INSULATED_ORANGE,     false, InsulatedRedAlloyWirePart::new, EnumColour.ORANGE,     1, "orange_insulated_wire_off",     "orange_insulated_wire_on"),
    INSULATED_MAGENTA   (ID_INSULATED_MAGENTA,    false, InsulatedRedAlloyWirePart::new, EnumColour.MAGENTA,    1, "magenta_insulated_wire_off",    "magenta_insulated_wire_on"),
    INSULATED_LIGHT_BLUE(ID_INSULATED_LIGHT_BLUE, false, InsulatedRedAlloyWirePart::new, EnumColour.LIGHT_BLUE, 1, "light_blue_insulated_wire_off", "light_blue_insulated_wire_on"),
    INSULATED_YELLOW    (ID_INSULATED_YELLOW,     false, InsulatedRedAlloyWirePart::new, EnumColour.YELLOW,     1, "yellow_insulated_wire_off",     "yellow_insulated_wire_on"),
    INSULATED_LIME      (ID_INSULATED_LIME,       false, InsulatedRedAlloyWirePart::new, EnumColour.LIME,       1, "lime_insulated_wire_off",       "lime_insulated_wire_on"),
    INSULATED_PINK      (ID_INSULATED_PINK,       false, InsulatedRedAlloyWirePart::new, EnumColour.PINK,       1, "pink_insulated_wire_off",       "pink_insulated_wire_on"),
    INSULATED_GRAY      (ID_INSULATED_GRAY,       false, InsulatedRedAlloyWirePart::new, EnumColour.GRAY,       1, "gray_insulated_wire_off",       "gray_insulated_wire_on"),
    INSULATED_LIGHT_GRAY(ID_INSULATED_LIGHT_GRAY, false, InsulatedRedAlloyWirePart::new, EnumColour.LIGHT_GRAY, 1, "light_gray_insulated_wire_off", "light_gray_insulated_wire_on"),
    INSULATED_CYAN      (ID_INSULATED_CYAN,       false, InsulatedRedAlloyWirePart::new, EnumColour.CYAN,       1, "cyan_insulated_wire_off",       "cyan_insulated_wire_on"),
    INSULATED_PURPLE    (ID_INSULATED_PURPLE,     false, InsulatedRedAlloyWirePart::new, EnumColour.PURPLE,     1, "purple_insulated_wire_off",     "purple_insulated_wire_on"),
    INSULATED_BLUE      (ID_INSULATED_BLUE,       false, InsulatedRedAlloyWirePart::new, EnumColour.BLUE,       1, "blue_insulated_wire_off",       "blue_insulated_wire_on"),
    INSULATED_BROWN     (ID_INSULATED_BROWN,      false, InsulatedRedAlloyWirePart::new, EnumColour.BROWN,      1, "brown_insulated_wire_off",      "brown_insulated_wire_on"),
    INSULATED_GREEN     (ID_INSULATED_GREEN,      false, InsulatedRedAlloyWirePart::new, EnumColour.GREEN,      1, "green_insulated_wire_off",      "green_insulated_wire_on"),
    INSULATED_RED       (ID_INSULATED_RED,        false, InsulatedRedAlloyWirePart::new, EnumColour.RED,        1, "red_insulated_wire_off",        "red_insulated_wire_on"),
    INSULATED_BLACK     (ID_INSULATED_BLACK,      false, InsulatedRedAlloyWirePart::new, EnumColour.BLACK,      1, "black_insulated_wire_off",      "black_insulated_wire_on"),

    BUNDLED_NEUTRAL   (ID_BUNDLED_NEUTRAL,    false, BundledCablePart::new,                        2, "neutral_bundled_wire"),
    BUNDLED_WHITE     (ID_BUNDLED_WHITE,      false, BundledCablePart::new, EnumColour.WHITE,      2, "white_bundled_wire"),
    BUNDLED_ORANGE    (ID_BUNDLED_ORANGE,     false, BundledCablePart::new, EnumColour.ORANGE,     2, "orange_bundled_wire"),
    BUNDLED_MAGENTA   (ID_BUNDLED_MAGENTA,    false, BundledCablePart::new, EnumColour.MAGENTA,    2, "magenta_bundled_wire"),
    BUNDLED_LIGHT_BLUE(ID_BUNDLED_LIGHT_BLUE, false, BundledCablePart::new, EnumColour.LIGHT_BLUE, 2, "light_blue_bundled_wire"),
    BUNDLED_YELLOW    (ID_BUNDLED_YELLOW,     false, BundledCablePart::new, EnumColour.YELLOW,     2, "yellow_bundled_wire"),
    BUNDLED_LIME      (ID_BUNDLED_LIME,       false, BundledCablePart::new, EnumColour.LIME,       2, "lime_bundled_wire"),
    BUNDLED_PINK      (ID_BUNDLED_PINK,       false, BundledCablePart::new, EnumColour.PINK,       2, "pink_bundled_wire"),
    BUNDLED_GRAY      (ID_BUNDLED_GRAY,       false, BundledCablePart::new, EnumColour.GRAY,       2, "gray_bundled_wire"),
    BUNDLED_LIGHT_GRAY(ID_BUNDLED_LIGHT_GRAY, false, BundledCablePart::new, EnumColour.LIGHT_GRAY, 2, "light_gray_bundled_wire"),
    BUNDLED_CYAN      (ID_BUNDLED_CYAN,       false, BundledCablePart::new, EnumColour.CYAN,       2, "cyan_bundled_wire"),
    BUNDLED_PURPLE    (ID_BUNDLED_PURPLE,     false, BundledCablePart::new, EnumColour.PURPLE,     2, "purple_bundled_wire"),
    BUNDLED_BLUE      (ID_BUNDLED_BLUE,       false, BundledCablePart::new, EnumColour.BLUE,       2, "blue_bundled_wire"),
    BUNDLED_BROWN     (ID_BUNDLED_BROWN,      false, BundledCablePart::new, EnumColour.BROWN,      2, "brown_bundled_wire"),
    BUNDLED_GREEN     (ID_BUNDLED_GREEN,      false, BundledCablePart::new, EnumColour.GREEN,      2, "green_bundled_wire"),
    BUNDLED_RED       (ID_BUNDLED_RED,        false, BundledCablePart::new, EnumColour.RED,        2, "red_bundled_wire"),
    BUNDLED_BLACK     (ID_BUNDLED_BLACK,      false, BundledCablePart::new, EnumColour.BLACK,      2, "black_bundled_wire"),

    POWER_LOWLOAD(ID_POWER_LOWLOAD, false, LowLoadPowerLine::new, 1, "low_load_power_wire"),

    FRAMED_RED_ALLOY(ID_FRAMED_RED_ALLOY, true, FramedRedAlloyWirePart::new, 0, 0xC80000, "red_alloy_wire"),

    FRAMED_INSULATED_WHITE     (ID_FRAMED_INSULATED_WHITE,      true, FramedInsulatedRedAlloyWirePart::new, EnumColour.WHITE,      1, "white_insulated_wire_off",      "white_insulated_wire_on"),
    FRAMED_INSULATED_ORANGE    (ID_FRAMED_INSULATED_ORANGE,     true, FramedInsulatedRedAlloyWirePart::new, EnumColour.ORANGE,     1, "orange_insulated_wire_off",     "orange_insulated_wire_on"),
    FRAMED_INSULATED_MAGENTA   (ID_FRAMED_INSULATED_MAGENTA,    true, FramedInsulatedRedAlloyWirePart::new, EnumColour.MAGENTA,    1, "magenta_insulated_wire_off",    "magenta_insulated_wire_on"),
    FRAMED_INSULATED_LIGHT_BLUE(ID_FRAMED_INSULATED_LIGHT_BLUE, true, FramedInsulatedRedAlloyWirePart::new, EnumColour.LIGHT_BLUE, 1, "light_blue_insulated_wire_off", "light_blue_insulated_wire_on"),
    FRAMED_INSULATED_YELLOW    (ID_FRAMED_INSULATED_YELLOW,     true, FramedInsulatedRedAlloyWirePart::new, EnumColour.YELLOW,     1, "yellow_insulated_wire_off",     "yellow_insulated_wire_on"),
    FRAMED_INSULATED_LIME      (ID_FRAMED_INSULATED_LIME,       true, FramedInsulatedRedAlloyWirePart::new, EnumColour.LIME,       1, "lime_insulated_wire_off",       "lime_insulated_wire_on"),
    FRAMED_INSULATED_PINK      (ID_FRAMED_INSULATED_PINK,       true, FramedInsulatedRedAlloyWirePart::new, EnumColour.PINK,       1, "pink_insulated_wire_off",       "pink_insulated_wire_on"),
    FRAMED_INSULATED_GRAY      (ID_FRAMED_INSULATED_GRAY,       true, FramedInsulatedRedAlloyWirePart::new, EnumColour.GRAY,       1, "gray_insulated_wire_off",       "gray_insulated_wire_on"),
    FRAMED_INSULATED_LIGHT_GRAY(ID_FRAMED_INSULATED_LIGHT_GRAY, true, FramedInsulatedRedAlloyWirePart::new, EnumColour.LIGHT_GRAY, 1, "light_gray_insulated_wire_off", "light_gray_insulated_wire_on"),
    FRAMED_INSULATED_CYAN      (ID_FRAMED_INSULATED_CYAN,       true, FramedInsulatedRedAlloyWirePart::new, EnumColour.CYAN,       1, "cyan_insulated_wire_off",       "cyan_insulated_wire_on"),
    FRAMED_INSULATED_PURPLE    (ID_FRAMED_INSULATED_PURPLE,     true, FramedInsulatedRedAlloyWirePart::new, EnumColour.PURPLE,     1, "purple_insulated_wire_off",     "purple_insulated_wire_on"),
    FRAMED_INSULATED_BLUE      (ID_FRAMED_INSULATED_BLUE,       true, FramedInsulatedRedAlloyWirePart::new, EnumColour.BLUE,       1, "blue_insulated_wire_off",       "blue_insulated_wire_on"),
    FRAMED_INSULATED_BROWN     (ID_FRAMED_INSULATED_BROWN,      true, FramedInsulatedRedAlloyWirePart::new, EnumColour.BROWN,      1, "brown_insulated_wire_off",      "brown_insulated_wire_on"),
    FRAMED_INSULATED_GREEN     (ID_FRAMED_INSULATED_GREEN,      true, FramedInsulatedRedAlloyWirePart::new, EnumColour.GREEN,      1, "green_insulated_wire_off",      "green_insulated_wire_on"),
    FRAMED_INSULATED_RED       (ID_FRAMED_INSULATED_RED,        true, FramedInsulatedRedAlloyWirePart::new, EnumColour.RED,        1, "red_insulated_wire_off",        "red_insulated_wire_on"),
    FRAMED_INSULATED_BLACK     (ID_FRAMED_INSULATED_BLACK,      true, FramedInsulatedRedAlloyWirePart::new, EnumColour.BLACK,      1, "black_insulated_wire_off",      "black_insulated_wire_on"),

    FRAMED_BUNDLED_NEUTRAL   (ID_FRAMED_BUNDLED_NEUTRAL,    true, FramedBundledCablePart::new,                        2, "neutral_bundled_wire"),
    FRAMED_BUNDLED_WHITE     (ID_FRAMED_BUNDLED_WHITE,      true, FramedBundledCablePart::new, EnumColour.WHITE,      2, "white_bundled_wire"),
    FRAMED_BUNDLED_ORANGE    (ID_FRAMED_BUNDLED_ORANGE,     true, FramedBundledCablePart::new, EnumColour.ORANGE,     2, "orange_bundled_wire"),
    FRAMED_BUNDLED_MAGENTA   (ID_FRAMED_BUNDLED_MAGENTA,    true, FramedBundledCablePart::new, EnumColour.MAGENTA,    2, "magenta_bundled_wire"),
    FRAMED_BUNDLED_LIGHT_BLUE(ID_FRAMED_BUNDLED_LIGHT_BLUE, true, FramedBundledCablePart::new, EnumColour.LIGHT_BLUE, 2, "light_blue_bundled_wire"),
    FRAMED_BUNDLED_YELLOW    (ID_FRAMED_BUNDLED_YELLOW,     true, FramedBundledCablePart::new, EnumColour.YELLOW,     2, "yellow_bundled_wire"),
    FRAMED_BUNDLED_LIME      (ID_FRAMED_BUNDLED_LIME,       true, FramedBundledCablePart::new, EnumColour.LIME,       2, "lime_bundled_wire"),
    FRAMED_BUNDLED_PINK      (ID_FRAMED_BUNDLED_PINK,       true, FramedBundledCablePart::new, EnumColour.PINK,       2, "pink_bundled_wire"),
    FRAMED_BUNDLED_GRAY      (ID_FRAMED_BUNDLED_GRAY,       true, FramedBundledCablePart::new, EnumColour.GRAY,       2, "gray_bundled_wire"),
    FRAMED_BUNDLED_LIGHT_GRAY(ID_FRAMED_BUNDLED_LIGHT_GRAY, true, FramedBundledCablePart::new, EnumColour.LIGHT_GRAY, 2, "light_gray_bundled_wire"),
    FRAMED_BUNDLED_CYAN      (ID_FRAMED_BUNDLED_CYAN,       true, FramedBundledCablePart::new, EnumColour.CYAN,       2, "cyan_bundled_wire"),
    FRAMED_BUNDLED_PURPLE    (ID_FRAMED_BUNDLED_PURPLE,     true, FramedBundledCablePart::new, EnumColour.PURPLE,     2, "purple_bundled_wire"),
    FRAMED_BUNDLED_BLUE      (ID_FRAMED_BUNDLED_BLUE,       true, FramedBundledCablePart::new, EnumColour.BLUE,       2, "blue_bundled_wire"),
    FRAMED_BUNDLED_BROWN     (ID_FRAMED_BUNDLED_BROWN,      true, FramedBundledCablePart::new, EnumColour.BROWN,      2, "brown_bundled_wire"),
    FRAMED_BUNDLED_GREEN     (ID_FRAMED_BUNDLED_GREEN,      true, FramedBundledCablePart::new, EnumColour.GREEN,      2, "green_bundled_wire"),
    FRAMED_BUNDLED_RED       (ID_FRAMED_BUNDLED_RED,        true, FramedBundledCablePart::new, EnumColour.RED,        2, "red_bundled_wire"),
    FRAMED_BUNDLED_BLACK     (ID_FRAMED_BUNDLED_BLACK,      true, FramedBundledCablePart::new, EnumColour.BLACK,      2, "black_bundled_wire"),

    FRAMED_POWER_LOWLOAD(ID_FRAMED_POWER_LOWLOAD, true, FramedLowLoadPowerLine::new, 1, "low_load_power_wire");
    //@formatter:on

    public static final WireType[] INSULATED_WIRES = { INSULATED_WHITE, INSULATED_ORANGE, INSULATED_MAGENTA, INSULATED_LIGHT_BLUE, INSULATED_YELLOW, INSULATED_LIME, INSULATED_PINK, INSULATED_GRAY, INSULATED_LIGHT_GRAY, INSULATED_CYAN, INSULATED_PURPLE, INSULATED_BLUE, INSULATED_BROWN, INSULATED_GREEN, INSULATED_RED, INSULATED_BLACK };
    public static final WireType[] COLOURED_BUNDLED_WIRES = { BUNDLED_WHITE, BUNDLED_ORANGE, BUNDLED_MAGENTA, BUNDLED_LIGHT_BLUE, BUNDLED_YELLOW, BUNDLED_LIME, BUNDLED_PINK, BUNDLED_GRAY, BUNDLED_LIGHT_GRAY, BUNDLED_CYAN, BUNDLED_PURPLE, BUNDLED_BLUE, BUNDLED_BROWN, BUNDLED_GREEN, BUNDLED_RED, BUNDLED_BLACK };
    public static final WireType[] FRAMED_INSULATED_WIRES = { FRAMED_INSULATED_WHITE, FRAMED_INSULATED_ORANGE, FRAMED_INSULATED_MAGENTA, FRAMED_INSULATED_LIGHT_BLUE, FRAMED_INSULATED_YELLOW, FRAMED_INSULATED_LIME, FRAMED_INSULATED_PINK, FRAMED_INSULATED_GRAY, FRAMED_INSULATED_LIGHT_GRAY, FRAMED_INSULATED_CYAN, FRAMED_INSULATED_PURPLE, FRAMED_INSULATED_BLUE, FRAMED_INSULATED_BROWN, FRAMED_INSULATED_GREEN, FRAMED_INSULATED_RED, FRAMED_INSULATED_BLACK };
    public static final WireType[] FRAMED_COLOURED_BUNDLED_WIRES = { FRAMED_BUNDLED_WHITE, FRAMED_BUNDLED_ORANGE, FRAMED_BUNDLED_MAGENTA, FRAMED_BUNDLED_LIGHT_BLUE, FRAMED_BUNDLED_YELLOW, FRAMED_BUNDLED_LIME, FRAMED_BUNDLED_PINK, FRAMED_BUNDLED_GRAY, FRAMED_BUNDLED_LIGHT_GRAY, FRAMED_BUNDLED_CYAN, FRAMED_BUNDLED_PURPLE, FRAMED_BUNDLED_BLUE, FRAMED_BUNDLED_BROWN, FRAMED_BUNDLED_GREEN, FRAMED_BUNDLED_RED, FRAMED_BUNDLED_BLACK };

    private final String unlocalName;
    private final boolean isCenterPart;
    private final Function<WireType, BaseWirePart> partFactory;
    private final @Nullable EnumColour colour;
    private final int thickness;
    private final int itemColour;
    private final List<String> textureNames;

    private @Nullable Supplier<Item> itemSupplier;
    private @Nullable Supplier<MultipartType<BaseWirePart>> partSupplier;

    @OnlyIn(Dist.CLIENT)
    private @Nullable List<TextureAtlasSprite> textures;

    WireType(String unlocalName, boolean isCenterPart, Function<WireType, BaseWirePart> partFactory, int thickness, String... textures) {
        this(unlocalName, isCenterPart, partFactory, null, thickness, textures);
    }

    WireType(String unlocalName, boolean isCenterPart, Function<WireType, BaseWirePart> partFactory, @Nullable EnumColour colour, int thickness, String... textures) {
        this(unlocalName, isCenterPart, partFactory, colour, thickness, 0xFFFFFF, textures);
    }

    WireType(String unlocalName, boolean isCenterPart, Function<WireType, BaseWirePart> partFactory, int thickness, int itemColour, String... textures) {
        this(unlocalName, isCenterPart, partFactory, null, thickness, itemColour, textures);
    }

    WireType(String unlocalName, boolean isCenterPart, Function<WireType, BaseWirePart> partFactory, @Nullable EnumColour colour, int thickness, int itemColour, String... textures) {
        this.unlocalName = unlocalName;
        this.isCenterPart = isCenterPart;
        this.partFactory = partFactory;
        this.colour = colour;
        this.thickness = thickness;
        this.itemColour = itemColour;
        this.textureNames = ImmutableList.copyOf(textures);
    }

    public String getUnlocalizedName() {
        return unlocalName;
    }

    public boolean isCenterPart() {
        return isCenterPart;
    }

    public Supplier<Item> getItemRegistryObject() {
        assert itemSupplier != null;
        return itemSupplier;
    }

    public Item getItem() {
        return Objects.requireNonNull(itemSupplier).get();
    }

    public ItemStack makeStack() {
        return new ItemStack(getItem());
    }

    public MultipartType<BaseWirePart> getPartType() {
        return Objects.requireNonNull(partSupplier).get();
    }

    public BaseWirePart newPart() {
        return partFactory.apply(this);
    }

    public EnumColour getColour() {
        return Objects.requireNonNull(colour);
    }

    public int getColourIdx() {
        return colour == null ? -1 : colour.getWoolMeta();
    }

    public int getThickness() {
        return thickness;
    }

    public int getItemColour() {
        return itemColour;
    }

    @OnlyIn (Dist.CLIENT)
    public List<TextureAtlasSprite> getTextures() {
        return Objects.requireNonNull(textures);
    }

    @OnlyIn (Dist.CLIENT)
    public void onTextureStitchEvent(TextureAtlasStitchedEvent event) {
        if (!event.getAtlas().location().equals(TextureAtlas.LOCATION_BLOCKS)) return;

        if (textureNames.isEmpty()) return;

        textures = new ArrayList<>(textureNames.size());
        for (int i = 0; i < textureNames.size(); i++) {
            textures.add(null);
        }
        for (int i = 0; i < textureNames.size(); i++) {
            ResourceLocation tex = new ResourceLocation(MOD_ID, "block/" + textureNames.get(i));
            textures.set(i, event.getAtlas().getSprite(tex));
        }
    }

    public void registerParts(DeferredRegister<MultipartType<?>> partRegistry, DeferredRegister<Item> itemRegistry) {
        itemSupplier = itemRegistry.register(unlocalName, isCenterPart ? () -> new CenterWirePartItem(this) : () -> new FaceWirePartItem(this));
        partSupplier = partRegistry.register(unlocalName, () -> new SimpleMultipartType<>(isClient -> partFactory.apply(this)));
    }
}
