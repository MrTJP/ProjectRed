package mrtjp.projectred.transmission;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.lib.util.SneakyUtils;
import codechicken.multipart.api.MultiPartType;
import codechicken.multipart.api.SimpleMultiPartType;
import codechicken.multipart.api.part.TMultiPart;
import com.google.common.collect.ImmutableList;
import mrtjp.projectred.ProjectRedTransmission;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Created by covers1624 on 22/12/20.
 */
public enum WireType implements SimpleMultiPartType.SimpleMultiPartTypeFactory<TMultiPart> {

    //@formatter:off
    RED_ALLOY(TransmissionContent::itemRedAlloyWire, TransmissionContent::partRedAlloyWire, RedAlloyWirePart::new, 0, 0xC80000, "redalloy"),
    INSULATED_WHITE     (TransmissionContent::itemInsulatedWhiteWire,     TransmissionContent::partInsulatedWhiteWire,     InsulatedRedAlloyPart::new, EnumColour.WHITE,      1, "insulated/whiteoff",     "insulated/whiteon"),
    INSULATED_ORANGE    (TransmissionContent::itemInsulatedOrangeWire,    TransmissionContent::partInsulatedOrangeWire,    InsulatedRedAlloyPart::new, EnumColour.ORANGE,     1, "insulated/orangeoff",    "insulated/orangeon"),
    INSULATED_MAGENTA   (TransmissionContent::itemInsulatedMagentaWire,   TransmissionContent::partInsulatedMagentaWire,   InsulatedRedAlloyPart::new, EnumColour.MAGENTA,    1, "insulated/magentaoff",   "insulated/magentaon"),
    INSULATED_LIGHT_BLUE(TransmissionContent::itemInsulatedLightBlueWire, TransmissionContent::partInsulatedLightBlueWire, InsulatedRedAlloyPart::new, EnumColour.LIGHT_BLUE, 1, "insulated/lightblueoff", "insulated/lightblueon"),
    INSULATED_YELLOW    (TransmissionContent::itemInsulatedYellowWire,    TransmissionContent::partInsulatedYellowWire,    InsulatedRedAlloyPart::new, EnumColour.YELLOW,     1, "insulated/yellowoff",    "insulated/yellowon"),
    INSULATED_LIME      (TransmissionContent::itemInsulatedLimeWire,      TransmissionContent::partInsulatedLimeWire,      InsulatedRedAlloyPart::new, EnumColour.LIME,       1, "insulated/limeoff",      "insulated/limeon"),
    INSULATED_PINK      (TransmissionContent::itemInsulatedPinkWire,      TransmissionContent::partInsulatedPinkWire,      InsulatedRedAlloyPart::new, EnumColour.PINK,       1, "insulated/pinkoff",      "insulated/pinkon"),
    INSULATED_GRAY      (TransmissionContent::itemInsulatedGrayWire,      TransmissionContent::partInsulatedGrayWire,      InsulatedRedAlloyPart::new, EnumColour.GRAY,       1, "insulated/greyoff",      "insulated/greyon"),
    INSULATED_LIGHT_GRAY(TransmissionContent::itemInsulatedLightGrayWire, TransmissionContent::partInsulatedLightGrayWire, InsulatedRedAlloyPart::new, EnumColour.LIGHT_GRAY, 1, "insulated/lightgreyoff", "insulated/lightgreyon"),
    INSULATED_CYAN      (TransmissionContent::itemInsulatedCyanWire,      TransmissionContent::partInsulatedCyanWire,      InsulatedRedAlloyPart::new, EnumColour.CYAN,       1, "insulated/cyanoff",      "insulated/cyanon"),
    INSULATED_PURPLE    (TransmissionContent::itemInsulatedPurpleWire,    TransmissionContent::partInsulatedPurpleWire,    InsulatedRedAlloyPart::new, EnumColour.PURPLE,     1, "insulated/purpleoff",    "insulated/purpleon"),
    INSULATED_BLUE      (TransmissionContent::itemInsulatedBlueWire,      TransmissionContent::partInsulatedBlueWire,      InsulatedRedAlloyPart::new, EnumColour.BLUE,       1, "insulated/blueoff",      "insulated/blueon"),
    INSULATED_BROWN     (TransmissionContent::itemInsulatedBrownWire,     TransmissionContent::partInsulatedBrownWire,     InsulatedRedAlloyPart::new, EnumColour.BROWN,      1, "insulated/brownoff",     "insulated/brownon"),
    INSULATED_GREEN     (TransmissionContent::itemInsulatedGreenWire,     TransmissionContent::partInsulatedGreenWire,     InsulatedRedAlloyPart::new, EnumColour.GREEN,      1, "insulated/greenoff",     "insulated/greenon"),
    INSULATED_RED       (TransmissionContent::itemInsulatedRedWire,       TransmissionContent::partInsulatedRedWire,       InsulatedRedAlloyPart::new, EnumColour.RED,        1, "insulated/redoff",       "insulated/redon"),
    INSULATED_BLACK     (TransmissionContent::itemInsulatedBlackWire,     TransmissionContent::partInsulatedBlackWire,     InsulatedRedAlloyPart::new, EnumColour.BLACK,      1, "insulated/blackoff",     "insulated/blackon"),

    BUNDLED_NEUTRAL   (TransmissionContent::itemBundledNeutralWire,   TransmissionContent::partBundledNeutralWire,   BundledCablePart::new,                        2, "bundled/neutral"),
    BUNDLED_WHITE     (TransmissionContent::itemBundledWhiteWire,     TransmissionContent::partBundledWhiteWire,     BundledCablePart::new, EnumColour.WHITE,      2, "bundled/white"),
    BUNDLED_ORANGE    (TransmissionContent::itemBundledOrangeWire,    TransmissionContent::partBundledOrangeWire,    BundledCablePart::new, EnumColour.ORANGE,     2, "bundled/orange"),
    BUNDLED_MAGENTA   (TransmissionContent::itemBundledMagentaWire,   TransmissionContent::partBundledMagentaWire,   BundledCablePart::new, EnumColour.MAGENTA,    2, "bundled/magenta"),
    BUNDLED_LIGHT_BLUE(TransmissionContent::itemBundledLightBlueWire, TransmissionContent::partBundledLightBlueWire, BundledCablePart::new, EnumColour.LIGHT_BLUE, 2, "bundled/lightblue"),
    BUNDLED_YELLOW    (TransmissionContent::itemBundledYellowWire,    TransmissionContent::partBundledYellowWire,    BundledCablePart::new, EnumColour.YELLOW,     2, "bundled/yellow"),
    BUNDLED_LIME      (TransmissionContent::itemBundledLimeWire,      TransmissionContent::partBundledLimeWire,      BundledCablePart::new, EnumColour.LIME,       2, "bundled/lime"),
    BUNDLED_PINK      (TransmissionContent::itemBundledPinkWire,      TransmissionContent::partBundledPinkWire,      BundledCablePart::new, EnumColour.PINK,       2, "bundled/pink"),
    BUNDLED_GRAY      (TransmissionContent::itemBundledGrayWire,      TransmissionContent::partBundledGrayWire,      BundledCablePart::new, EnumColour.GRAY,       2, "bundled/grey"),
    BUNDLED_LIGHT_GRAY(TransmissionContent::itemBundledLightGrayWire, TransmissionContent::partBundledLightGrayWire, BundledCablePart::new, EnumColour.LIGHT_GRAY, 2, "bundled/lightgrey"),
    BUNDLED_CYAN      (TransmissionContent::itemBundledCyanWire,      TransmissionContent::partBundledCyanWire,      BundledCablePart::new, EnumColour.CYAN,       2, "bundled/cyan"),
    BUNDLED_PURPLE    (TransmissionContent::itemBundledPurpleWire,    TransmissionContent::partBundledPurpleWire,    BundledCablePart::new, EnumColour.PURPLE,     2, "bundled/purple"),
    BUNDLED_BLUE      (TransmissionContent::itemBundledBlueWire,      TransmissionContent::partBundledBlueWire,      BundledCablePart::new, EnumColour.BLUE,       2, "bundled/blue"),
    BUNDLED_BROWN     (TransmissionContent::itemBundledBrownWire,     TransmissionContent::partBundledBrownWire,     BundledCablePart::new, EnumColour.BROWN,      2, "bundled/brown"),
    BUNDLED_GREEN     (TransmissionContent::itemBundledGreenWire,     TransmissionContent::partBundledGreenWire,     BundledCablePart::new, EnumColour.GREEN,      2, "bundled/green"),
    BUNDLED_RED       (TransmissionContent::itemBundledRedWire,       TransmissionContent::partBundledRedWire,       BundledCablePart::new, EnumColour.RED,        2, "bundled/red"),
    BUNDLED_BLACK     (TransmissionContent::itemBundledBlackWire,     TransmissionContent::partBundledBlackWire,     BundledCablePart::new, EnumColour.BLACK,      2, "bundled/black"),

    POWER_LOWLOAD(TransmissionContent::itemPowerLowLoadWire, TransmissionContent::partPowerLowLoadWire, LowLoadPowerLine::new, 1, "power/lowload"),

    FRAMED_RED_ALLOY(TransmissionContent::itemFramedRedAlloyWire, TransmissionContent::partFramedRedAlloyWire, FramedRedAlloyWirePart::new, 0, 0xC80000, "redalloy"),

    FRAMED_INSULATED_WHITE     (TransmissionContent::itemFramedInsulatedWhiteWire,     TransmissionContent::partFramedInsulatedWhiteWire,     FramedInsulatedRedAlloyPart::new, EnumColour.WHITE,      1, "insulated/whiteoff",     "insulated/whiteon"),
    FRAMED_INSULATED_ORANGE    (TransmissionContent::itemFramedInsulatedOrangeWire,    TransmissionContent::partFramedInsulatedOrangeWire,    FramedInsulatedRedAlloyPart::new, EnumColour.ORANGE,     1, "insulated/orangeoff",    "insulated/orangeon"),
    FRAMED_INSULATED_MAGENTA   (TransmissionContent::itemFramedInsulatedMagentaWire,   TransmissionContent::partFramedInsulatedMagentaWire,   FramedInsulatedRedAlloyPart::new, EnumColour.MAGENTA,    1, "insulated/magentaoff",   "insulated/magentaon"),
    FRAMED_INSULATED_LIGHT_BLUE(TransmissionContent::itemFramedInsulatedLightBlueWire, TransmissionContent::partFramedInsulatedLightBlueWire, FramedInsulatedRedAlloyPart::new, EnumColour.LIGHT_BLUE, 1, "insulated/lightblueoff", "insulated/lightblueon"),
    FRAMED_INSULATED_YELLOW    (TransmissionContent::itemFramedInsulatedYellowWire,    TransmissionContent::partFramedInsulatedYellowWire,    FramedInsulatedRedAlloyPart::new, EnumColour.YELLOW,     1, "insulated/yellowoff",    "insulated/yellowon"),
    FRAMED_INSULATED_LIME      (TransmissionContent::itemFramedInsulatedLimeWire,      TransmissionContent::partFramedInsulatedLimeWire,      FramedInsulatedRedAlloyPart::new, EnumColour.LIME,       1, "insulated/limeoff",      "insulated/limeon"),
    FRAMED_INSULATED_PINK      (TransmissionContent::itemFramedInsulatedPinkWire,      TransmissionContent::partFramedInsulatedPinkWire,      FramedInsulatedRedAlloyPart::new, EnumColour.PINK,       1, "insulated/pinkoff",      "insulated/pinkon"),
    FRAMED_INSULATED_GRAY      (TransmissionContent::itemFramedInsulatedGrayWire,      TransmissionContent::partFramedInsulatedGrayWire,      FramedInsulatedRedAlloyPart::new, EnumColour.GRAY,       1, "insulated/greyoff",      "insulated/greyon"),
    FRAMED_INSULATED_LIGHT_GRAY(TransmissionContent::itemFramedInsulatedLightGrayWire, TransmissionContent::partFramedInsulatedLightGrayWire, FramedInsulatedRedAlloyPart::new, EnumColour.LIGHT_GRAY, 1, "insulated/lightgreyoff", "insulated/lightgreyon"),
    FRAMED_INSULATED_CYAN      (TransmissionContent::itemFramedInsulatedCyanWire,      TransmissionContent::partFramedInsulatedCyanWire,      FramedInsulatedRedAlloyPart::new, EnumColour.CYAN,       1, "insulated/cyanoff",      "insulated/cyanon"),
    FRAMED_INSULATED_PURPLE    (TransmissionContent::itemFramedInsulatedPurpleWire,    TransmissionContent::partFramedInsulatedPurpleWire,    FramedInsulatedRedAlloyPart::new, EnumColour.PURPLE,     1, "insulated/purpleoff",    "insulated/purpleon"),
    FRAMED_INSULATED_BLUE      (TransmissionContent::itemFramedInsulatedBlueWire,      TransmissionContent::partFramedInsulatedBlueWire,      FramedInsulatedRedAlloyPart::new, EnumColour.BLUE,       1, "insulated/blueoff",      "insulated/blueon"),
    FRAMED_INSULATED_BROWN     (TransmissionContent::itemFramedInsulatedBrownWire,     TransmissionContent::partFramedInsulatedBrownWire,     FramedInsulatedRedAlloyPart::new, EnumColour.BROWN,      1, "insulated/brownoff",     "insulated/brownon"),
    FRAMED_INSULATED_GREEN     (TransmissionContent::itemFramedInsulatedGreenWire,     TransmissionContent::partFramedInsulatedGreenWire,     FramedInsulatedRedAlloyPart::new, EnumColour.GREEN,      1, "insulated/greenoff",     "insulated/greenon"),
    FRAMED_INSULATED_RED       (TransmissionContent::itemFramedInsulatedRedWire,       TransmissionContent::partFramedInsulatedRedWire,       FramedInsulatedRedAlloyPart::new, EnumColour.RED,        1, "insulated/redoff",       "insulated/redon"),
    FRAMED_INSULATED_BLACK     (TransmissionContent::itemFramedInsulatedBlackWire,     TransmissionContent::partFramedInsulatedBlackWire,     FramedInsulatedRedAlloyPart::new, EnumColour.BLACK,      1, "insulated/blackoff",     "insulated/blackon"),

    FRAMED_BUNDLED_NEUTRAL   (TransmissionContent::itemFramedBundledNeutralWire,   TransmissionContent::partFramedBundledNeutralWire, FramedBundledCablePart::new, 2, "bundled/neutral"),
    FRAMED_BUNDLED_WHITE     (TransmissionContent::itemFramedBundledWhiteWire,     TransmissionContent::partFramedBundledWhiteWire,     FramedBundledCablePart::new, EnumColour.WHITE,      2, "bundled/white"),
    FRAMED_BUNDLED_ORANGE    (TransmissionContent::itemFramedBundledOrangeWire,    TransmissionContent::partFramedBundledOrangeWire,    FramedBundledCablePart::new, EnumColour.ORANGE,     2, "bundled/orange"),
    FRAMED_BUNDLED_MAGENTA   (TransmissionContent::itemFramedBundledMagentaWire,   TransmissionContent::partFramedBundledMagentaWire,   FramedBundledCablePart::new, EnumColour.MAGENTA,    2, "bundled/magenta"),
    FRAMED_BUNDLED_LIGHT_BLUE(TransmissionContent::itemFramedBundledLightBlueWire, TransmissionContent::partFramedBundledLightBlueWire, FramedBundledCablePart::new, EnumColour.LIGHT_BLUE, 2, "bundled/lightblue"),
    FRAMED_BUNDLED_YELLOW    (TransmissionContent::itemFramedBundledYellowWire,    TransmissionContent::partFramedBundledYellowWire,    FramedBundledCablePart::new, EnumColour.YELLOW,     2, "bundled/yellow"),
    FRAMED_BUNDLED_LIME      (TransmissionContent::itemFramedBundledLimeWire,      TransmissionContent::partFramedBundledLimeWire,      FramedBundledCablePart::new, EnumColour.LIME,       2, "bundled/lime"),
    FRAMED_BUNDLED_PINK      (TransmissionContent::itemFramedBundledPinkWire,      TransmissionContent::partFramedBundledPinkWire,      FramedBundledCablePart::new, EnumColour.PINK,       2, "bundled/pink"),
    FRAMED_BUNDLED_GRAY      (TransmissionContent::itemFramedBundledGrayWire,      TransmissionContent::partFramedBundledGrayWire,      FramedBundledCablePart::new, EnumColour.GRAY,       2, "bundled/grey"),
    FRAMED_BUNDLED_LIGHT_GRAY(TransmissionContent::itemFramedBundledLightGrayWire, TransmissionContent::partFramedBundledLightGrayWire, FramedBundledCablePart::new, EnumColour.LIGHT_GRAY, 2, "bundled/lightgrey"),
    FRAMED_BUNDLED_CYAN      (TransmissionContent::itemFramedBundledCyanWire,      TransmissionContent::partFramedBundledCyanWire,      FramedBundledCablePart::new, EnumColour.CYAN,       2, "bundled/cyan"),
    FRAMED_BUNDLED_PURPLE    (TransmissionContent::itemFramedBundledPurpleWire,    TransmissionContent::partFramedBundledPurpleWire,    FramedBundledCablePart::new, EnumColour.PURPLE,     2, "bundled/purple"),
    FRAMED_BUNDLED_BLUE      (TransmissionContent::itemFramedBundledBlueWire,      TransmissionContent::partFramedBundledBlueWire,      FramedBundledCablePart::new, EnumColour.BLUE,       2, "bundled/blue"),
    FRAMED_BUNDLED_BROWN     (TransmissionContent::itemFramedBundledBrownWire,     TransmissionContent::partFramedBundledBrownWire,     FramedBundledCablePart::new, EnumColour.BROWN,      2, "bundled/brown"),
    FRAMED_BUNDLED_GREEN     (TransmissionContent::itemFramedBundledGreenWire,     TransmissionContent::partFramedBundledGreenWire,     FramedBundledCablePart::new, EnumColour.GREEN,      2, "bundled/green"),
    FRAMED_BUNDLED_RED       (TransmissionContent::itemFramedBundledRedWire,       TransmissionContent::partFramedBundledRedWire,       FramedBundledCablePart::new, EnumColour.RED,        2, "bundled/red"),
    FRAMED_BUNDLED_BLACK     (TransmissionContent::itemFramedBundledBlackWire,     TransmissionContent::partFramedBundledBlackWire,     FramedBundledCablePart::new, EnumColour.BLACK,      2, "bundled/black"),

    FRAMED_POWER_LOWLOAD(TransmissionContent::itemFramedPowerLowLoadWire, TransmissionContent::partFramedPowerLowLoadWire, FramedLowLoadPowerLine::new, 1, "power/lowload");
    //@formatter:on

    private final Supplier<Supplier<? extends ItemPartWire>> itemSupplier;
    private final Supplier<Supplier<MultiPartType<?>>> partSupplier;
    private final Function<WireType, TWireCommons> partFactory;
    private final EnumColour colour;
    private final int thickness;
    private final int itemColour;
    private final List<String> textureNames;

    @OnlyIn (Dist.CLIENT)
    private List<TextureAtlasSprite> textures;
    private ItemPartWire item;
    private MultiPartType<?> partType;

    WireType(Supplier<Supplier<? extends ItemPartWire>> itemSupplier, Supplier<Supplier<MultiPartType<?>>> partSupplier, Function<WireType, TWireCommons> partFactory, int thickness, String... textures) {
        this(itemSupplier, partSupplier, partFactory, null, thickness, textures);
    }

    WireType(Supplier<Supplier<? extends ItemPartWire>> itemSupplier, Supplier<Supplier<MultiPartType<?>>> partSupplier, Function<WireType, TWireCommons> partFactory, EnumColour colour, int thickness, String... textures) {
        this(itemSupplier, partSupplier, partFactory, colour, thickness, 0xFFFFFF, textures);
    }

    WireType(Supplier<Supplier<? extends ItemPartWire>> itemSupplier, Supplier<Supplier<MultiPartType<?>>> partSupplier, Function<WireType, TWireCommons> partFactory, int thickness, int itemColour, String... textures) {
        this(itemSupplier, partSupplier, partFactory, null, thickness, itemColour, textures);
    }

    WireType(Supplier<Supplier<? extends ItemPartWire>> itemSupplier, Supplier<Supplier<MultiPartType<?>>> partSupplier, Function<WireType, TWireCommons> partFactory, EnumColour colour, int thickness, int itemColour, String... textures) {
        this.itemSupplier = itemSupplier;
        this.partSupplier = partSupplier;
        this.partFactory = partFactory;
        this.colour = colour;
        this.thickness = thickness;
        this.itemColour = itemColour;
        this.textureNames = ImmutableList.copyOf(textures);
    }

    public ItemPartWire getItem() {
        if (item == null) {
            item = itemSupplier.get().get();
        }
        return item;
    }

    public ItemStack makeStack() {
        return new ItemStack(getItem());
    }

    public MultiPartType<?> getPartType() {
        if (partType == null) {
            partType = partSupplier.get().get();
        }
        return partType;
    }

    public TWireCommons newPart() {
        return partFactory.apply(this);
    }

    @Override
    public TMultiPart create(boolean client) {
        return SneakyUtils.unsafeCast(newPart());
    }

    public EnumColour getColour() {
        return colour;
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
        return textures;
    }

    @OnlyIn (Dist.CLIENT)
    void registerTextures(SpriteRegistryHelper spriteHelper) {
        if (textureNames.isEmpty()) {
            return;
        }
        textures = new ArrayList<>(textureNames.size());
        for (int i = 0; i < textureNames.size(); i++) {
            textures.add(null);
        }
        spriteHelper.addIIconRegister(SpriteRegistryHelper.TEXTURES, registrar -> {
            for (int i = 0; i < textureNames.size(); i++) {
                int finalI = i;
                ResourceLocation tex = new ResourceLocation(ProjectRedTransmission.MOD_ID(), "block/" + textureNames.get(i));
                registrar.registerSprite(tex, sprite -> textures.set(finalI, sprite));
            }
        });
    }
}
