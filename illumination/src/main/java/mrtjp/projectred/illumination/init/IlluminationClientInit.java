package mrtjp.projectred.illumination.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.multipart.api.MultipartClientRegistry;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.mojang.datafixers.util.Pair;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import mrtjp.projectred.illumination.client.IllumarLampItemRenderer;
import mrtjp.projectred.illumination.client.IllumarLampTileRenderer;
import mrtjp.projectred.illumination.client.MultipartLightPartRenderer;
import net.covers1624.quack.util.SneakyUtils;
import net.minecraft.client.renderer.block.model.ItemOverrides;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderers;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.model.IModelConfiguration;
import net.minecraftforge.client.model.IModelLoader;
import net.minecraftforge.client.model.ModelLoaderRegistry;
import net.minecraftforge.client.model.geometry.IModelGeometry;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

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
                ResourceLocation blockRL = Objects.requireNonNull(BlockLightType.ILLUMAR_LAMP.getBlock(color, true).getRegistryName());
                // Override default BlockItem renderer for the lit variants to render lamp glow
                BakedModel litModel = e.getModelRegistry().get(new ModelResourceLocation(blockRL, "lit=true"));
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
                //TODO this only works because Illumar lamp is the only BlockLightType
                BlockEntityRenderers.register(SneakyUtils.unsafeCast(lampType.getTileEntityType(color, false)), c ->  IllumarLampTileRenderer.INSTANCE);
                BlockEntityRenderers.register(SneakyUtils.unsafeCast(lampType.getTileEntityType(color, true)), c -> IllumarLampTileRenderer.INSTANCE);
            }
        }

        // Register part renderers
        for (MultipartLightType type : MultipartLightType.values()) {
            for (int colour = 0; colour < 16; colour++) {
                MultipartClientRegistry.register(type.getPartType(colour, false), MultipartLightPartRenderer.INSTANCE);
                MultipartClientRegistry.register(type.getPartType(colour, true), MultipartLightPartRenderer.INSTANCE);
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
        public BakedModel bake(IModelConfiguration owner, ModelBakery bakery, Function<Material, TextureAtlasSprite> spriteGetter, ModelState modelTransform, ItemOverrides overrides, ResourceLocation modelLocation) {
            return renderer;
        }

        //@formatter:off
        @Override public GenericModelLoader read(JsonDeserializationContext deserializationContext, JsonObject modelContents) { return this; }
        public Collection<Material> getTextures(IModelConfiguration owner, Function<ResourceLocation, UnbakedModel> modelGetter, Set<Pair<String, String>> missingTextureErrors) { return Collections.emptySet(); }
        @Override public void onResourceManagerReload(ResourceManager resourceManager) {}
        //@formatter:on
    }
}
