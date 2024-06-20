package mrtjp.projectred.expansion;

import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import com.google.common.collect.ImmutableList;
import mrtjp.projectred.expansion.item.TubePartItem;
import mrtjp.projectred.expansion.part.BaseTubePart;
import mrtjp.projectred.expansion.part.PneumaticTubePart;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionParts.ID_PNEUMATIC_TUBE;

public enum TubeType {

    PNEUMATIC_TUBE(ID_PNEUMATIC_TUBE, PneumaticTubePart::new, "pneumatic_tube"),
    ;

    private final String unlocalName;
    private final Function<TubeType, BaseTubePart> partFactory;
    private final List<String> textureNames;


    private @Nullable RegistryObject<Item> itemSupplier;
    private @Nullable RegistryObject<MultipartType<BaseTubePart>> partSupplier;

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

    public RegistryObject<Item> getItemRegistryObject() {
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
    public void registerTextures(SpriteRegistryHelper spriteHelper) {
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
                ResourceLocation tex = new ResourceLocation(MOD_ID, "block/" + textureNames.get(i));
                registrar.registerSprite(tex, sprite -> textures.set(finalI, sprite));
            }
        });
    }

    public void registerParts(DeferredRegister<MultipartType<?>> partRegistry, DeferredRegister<Item> itemRegistry) {
        itemSupplier = itemRegistry.register(unlocalName, () -> new TubePartItem(this));
        partSupplier = partRegistry.register(unlocalName, () -> new SimpleMultipartType<>(isClient -> partFactory.apply(this)));
    }
}
