package mrtjp.projectred.illumination;

import codechicken.lib.colour.EnumColour;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import mrtjp.projectred.illumination.item.MultipartLightPartItem;
import mrtjp.projectred.illumination.part.*;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;

import java.util.ArrayList;
import java.util.List;

public enum MultipartLightType {
    FIXTURE("fixture_light", "Fixture Light", new FixtureLightProperties()),
    FALLOUT("fallout_light", "Fallout Light", new FalloutLightProperties()),
    CAGE("cage_light", "Cage Light", new CageLightProperties()),
    LANTERN("lantern", "Lantern", new LanternLightProperties()),
    ;

    private final String unlocalName;
    private final String localName;
    private final MultipartLightProperties properties;

    private final List<RegistryObject<Item>> itemSuppliers = new ArrayList<>();
    private final List<RegistryObject<Item>> invertedItemSuppliers = new ArrayList<>();

    private final List<RegistryObject<MultipartType<MultipartLightPart>>> partSuppliers = new ArrayList<>();
    private final List<RegistryObject<MultipartType<MultipartLightPart>>> invertedPartSuppliers = new ArrayList<>();

    MultipartLightType(String unlocalName, String localName, MultipartLightProperties properties) {
        this.unlocalName = unlocalName;
        this.localName = localName;
        this.properties = properties;
    }

    public MultipartLightProperties getProperties() {
        return properties;
    }

    public void registerParts(DeferredRegister<MultipartType<?>> partRegistry, DeferredRegister<Item> itemRegistry) {
        // Non-inverted
        for (int color = 0; color < 16; color++) {
            final int colorFinal = color;
            String regID = getRegistryID(unlocalName, color, false);
            itemSuppliers.add(color, itemRegistry.register(regID, () -> new MultipartLightPartItem(properties, colorFinal, false)));
            partSuppliers.add(color, partRegistry.register(regID, () -> new SimpleMultipartType<>(isClient -> properties.partFactory(colorFinal, false))));
        }

        // Inverted
        for (int color = 0; color < 16; color++) {
            final int colorFinal = color;
            String invRegID = getRegistryID(unlocalName, color, true);
            invertedItemSuppliers.add(color, itemRegistry.register(invRegID, () -> new MultipartLightPartItem(properties, colorFinal, true)));
            invertedPartSuppliers.add(color, partRegistry.register(invRegID, () -> new SimpleMultipartType<>(isClient -> properties.partFactory(colorFinal, true))));
        }
    }

    public RegistryObject<Item> getItemRegistryObject(int color, boolean inverted) {
        return inverted ? invertedItemSuppliers.get(color) : itemSuppliers.get(color);
    }

    public Item getItem(int color, boolean inverted) {
        return inverted ? invertedItemSuppliers.get(color).get() : itemSuppliers.get(color).get();
    }

    public ItemStack makeStack(int colour, boolean inverted) {
        Item item = (inverted ? invertedItemSuppliers : itemSuppliers).get(colour).get();
        return new ItemStack(item);
    }

    public MultipartType<MultipartLightPart> getPartType(int color, boolean inverted) {
        return inverted ? invertedPartSuppliers.get(color).get() : partSuppliers.get(color).get();
    }

    public String getLocalBaseName() {
        return localName;
    }

    public String getUnlocalBaseName() {
        return unlocalName;
    }

    public String getRegistryID(int color, boolean inverted) {
        return getRegistryID(unlocalName, color, inverted);
    }

    private static String getRegistryID(String name, int color, boolean inverted) {
        return EnumColour.values()[color].getSerializedName() + (inverted ? "_inverted" : "") + "_" + name;
    }
}
