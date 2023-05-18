package mrtjp.projectred.transmission.init;

import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.lib.util.SneakyUtils;
import codechicken.microblock.MicroMaterialRegistry;
import codechicken.multipart.api.MultipartClientRegistry;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.datafixers.util.Pair;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.client.*;
import net.minecraft.client.renderer.model.*;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.IResourceManager;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.model.IModelConfiguration;
import net.minecraftforge.client.model.IModelLoader;
import net.minecraftforge.client.model.ModelLoaderRegistry;
import net.minecraftforge.client.model.geometry.IModelGeometry;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.resource.IResourceType;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;

public class TransmissionClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(TransmissionClientInit::clientSetup);
        modEventBus.addListener(TransmissionClientInit::onModelRegistryEvent);

        // Register sprites
        SpriteRegistryHelper spriteHelper = new SpriteRegistryHelper(modEventBus);
        for (WireType type : WireType.values()) {
            type.registerTextures(spriteHelper);
        }
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register part item renderers
        for (WireType type : WireType.values()) {
            MultipartClientRegistry.register(type.getPartType(),
                    SneakyUtils.unsafeCast(type.isCenterPart() ? CenterWirePartRenderer.INSTANCE : FaceWirePartRenderer.INSTANCE));
        }

        // Highlight renderer for cover-on-wire overlay
        MicroMaterialRegistry.registerHighlightRenderer(FramedWireHighlightRenderer.INSTANCE);
    }

    private static void onModelRegistryEvent(ModelRegistryEvent event) {

        // Register part model loaders
        ModelLoaderRegistry.registerLoader(new ResourceLocation(MOD_ID, "wire"),  new GenericModelLoader(WirePartItemRenderer.INSTANCE));
        ModelLoaderRegistry.registerLoader(new ResourceLocation(MOD_ID, "framed_wire"), new GenericModelLoader(FramedWirePartItemRenderer.INSTANCE));
    }

    // Dummy model loader that represents an IItemRenderer
    private static class GenericModelLoader implements IModelGeometry<GenericModelLoader>, IModelLoader<GenericModelLoader> {

        private final IItemRenderer renderer;

        public GenericModelLoader(IItemRenderer renderer) {
            this.renderer = renderer;
        }

        @Override
        public IBakedModel bake(IModelConfiguration owner, ModelBakery bakery, Function<RenderMaterial, TextureAtlasSprite> spriteGetter, IModelTransform modelTransform, ItemOverrideList overrides, ResourceLocation modelLocation) {
            return renderer;
        }

        //@formatter:off
        @Override public GenericModelLoader read(JsonDeserializationContext deserializationContext, JsonObject modelContents) { return this; }
        @Override public Collection<RenderMaterial> getTextures(IModelConfiguration owner, Function<ResourceLocation, IUnbakedModel> modelGetter, Set<Pair<String, String>> missingTextureErrors) { return Collections.emptyList(); }
        @Override public void onResourceManagerReload(IResourceManager resourceManager) {}
        @Override public void onResourceManagerReload(IResourceManager resourceManager, Predicate<IResourceType> resourcePredicate) {}
        //@formatter:on
    }
}
