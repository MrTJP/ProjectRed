package mrtjp.projectred.expansion;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import com.google.common.collect.ImmutableList;
import mrtjp.projectred.expansion.item.TubePartItem;
import mrtjp.projectred.expansion.part.BaseTubePart;
import mrtjp.projectred.expansion.part.PneumaticTubePart;
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

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionParts.ID_PNEUMATIC_TUBE;

public enum TubeType {

    PNEUMATIC_TUBE(ID_PNEUMATIC_TUBE, PneumaticTubePart::new, "pneumatic_tube"),
    ;

    private final String unlocalName;
    private final Function<TubeType, BaseTubePart> partFactory;
    private final List<String> textureNames;


    private @Nullable Supplier<Item> itemSupplier;
    private @Nullable Supplier<MultipartType<BaseTubePart>> partSupplier;

    @OnlyIn(Dist.CLIENT)
    private @Nullable List<TextureAtlasSprite> textures;

    TubeType(String unlocalName, Function<TubeType, BaseTubePart> partFactory, String... textures) {
        this.unlocalName = unlocalName;
        this.partFactory = partFactory;
        this.textureNames = ImmutableList.copyOf(textures);
    }

    public String getUnlocalizedName() {
        return unlocalName;
    }

    public Item getItem() {
        return Objects.requireNonNull(itemSupplier).get();
    }

    public ItemStack makeStack() {
        return new ItemStack(getItem());
    }

    public MultipartType<BaseTubePart> getPartType() {
        return Objects.requireNonNull(partSupplier).get();
    }

    public Supplier<Item> getItemRegistryObject() {
        return Objects.requireNonNull(itemSupplier);
    }

    public BaseTubePart newPart() {
        return partFactory.apply(this);
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
            ResourceLocation tex = ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/" + textureNames.get(i));
            textures.set(i, event.getAtlas().getSprite(tex));
        }
    }

    public void registerParts(DeferredRegister<MultipartType<?>> partRegistry, DeferredRegister<Item> itemRegistry) {
        itemSupplier = itemRegistry.register(unlocalName, () -> new TubePartItem(this));
        partSupplier = partRegistry.register(unlocalName, () -> new SimpleMultipartType<>(isClient -> partFactory.apply(this)));
    }
}
