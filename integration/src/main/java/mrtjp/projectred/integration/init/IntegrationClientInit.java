package mrtjp.projectred.integration.init;

import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.lib.util.ResourceUtils;
import codechicken.multipart.api.MultipartClientRegistry;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.datafixers.util.Pair;
import mrtjp.projectred.integration.ProjectRedIntegration;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateModelRenderer;
import mrtjp.projectred.integration.client.GatePartItemRenderer;
import mrtjp.projectred.integration.client.GatePartRenderer;
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

public class IntegrationClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(IntegrationClientInit::clientSetup);
        modEventBus.addListener(IntegrationClientInit::onModelRegistryEvent);

        // Register sprites
        SpriteRegistryHelper spriteHelper = new SpriteRegistryHelper(modEventBus);
        spriteHelper.addIIconRegister(GateModelRenderer::registerIcons);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register part item renderers
        for (GateType type : GateType.values()) {
            MultipartClientRegistry.register(type.getPartType(), GatePartRenderer.INSTANCE);
        }

        ResourceUtils.registerReloadListener(GateModelRenderer::onResourceManagerReload);
    }

    private static void onModelRegistryEvent(ModelRegistryEvent event) {

        // Register part model loader
        ModelLoaderRegistry.registerLoader(new ResourceLocation(ProjectRedIntegration.MOD_ID, "gate"), new GenericModelLoader(GatePartItemRenderer.INSTANCE));
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
