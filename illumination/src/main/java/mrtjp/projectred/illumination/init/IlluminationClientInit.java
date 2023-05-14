package mrtjp.projectred.illumination.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.lib.util.SneakyUtils;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.datafixers.util.Pair;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import mrtjp.projectred.illumination.client.IllumarLampItemRenderer;
import mrtjp.projectred.illumination.client.IllumarLampTileRenderer;
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
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.resource.IResourceType;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

import static mrtjp.projectred.ProjectRedIllumination.MOD_ID;

public class IlluminationClientInit {
    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(IlluminationClientInit::clientSetup);
        modEventBus.addListener(IlluminationClientInit::onModelRegistryEvent);

        // Register Sprites
        SpriteRegistryHelper iconRegister = new SpriteRegistryHelper();
        for (MultipartLightType type : MultipartLightType.values()) {
            iconRegister.addIIconRegister(type.getProperties()::registerIcons);
        }

        // Register custom block item models
        ModelRegistryHelper modelHelper = new ModelRegistryHelper();
        modelHelper.registerCallback(e -> {
            // Illumar lamp renderer
            for (int color = 0; color < 16; color++) {
                ResourceLocation blockRL = BlockLightType.ILLUMAR_LAMP.getBlock(color, true).getRegistryName();
                // Override default BlockItem renderer for the lit variants to render lamp glow
                IBakedModel litModel = e.getModelRegistry().get(new ModelResourceLocation(blockRL, "lit=true"));
                e.getModelRegistry().put(
                        new ModelResourceLocation(blockRL, "inventory"),
                        new IllumarLampItemRenderer(litModel));
            }
        });
    }

    private static void clientSetup(final FMLClientSetupEvent event) {
        // Bind Tile entity renderers
        for (BlockLightType lampType : BlockLightType.values()) {
            for (int color = 0; color < 16; color++) {
                ClientRegistry.bindTileEntityRenderer(SneakyUtils.unsafeCast(lampType.getTileEntityType(color, false)), IllumarLampTileRenderer::new);
                ClientRegistry.bindTileEntityRenderer(SneakyUtils.unsafeCast(lampType.getTileEntityType(color, true)), IllumarLampTileRenderer::new);
            }
        }
    }

    private static void onModelRegistryEvent(ModelRegistryEvent event) {
        // Add model loader that points to an IItemRenderer instance
        for (MultipartLightType type : MultipartLightType.values()) {
            ModelLoaderRegistry.registerLoader(
                    new ResourceLocation(MOD_ID, type.getUnlocalBaseName()),
                    new GenericModelLoader(type.getProperties().getItemRenderer()));
        }
    }

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
