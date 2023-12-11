package mrtjp.projectred.illumination.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import mrtjp.projectred.illumination.client.IllumarLampItemRenderer;
import mrtjp.projectred.illumination.client.IllumarLampTileRenderer;
import mrtjp.projectred.illumination.client.MultipartLightPartRenderer;
import net.covers1624.quack.util.SneakyUtils;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderers;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.Objects;

public class IlluminationClientInit {

    private static final ModelRegistryHelper MODEL_HELPER = new ModelRegistryHelper();

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(IlluminationClientInit::clientSetup);

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
                ResourceLocation blockRL = Objects.requireNonNull(ForgeRegistries.BLOCKS.getKey(BlockLightType.ILLUMAR_LAMP.getBlock(color, true)));
                // Override default BlockItem renderer for the lit variants to render lamp glow
                BakedModel litModel = e.getModels().get(new ModelResourceLocation(blockRL, "lit=true"));
                e.getModels().put(
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

        // Register light part renderers
        for (MultipartLightType type : MultipartLightType.values()) {
            for (int colour = 0; colour < 16; colour++) {
                // Block renderers
                MultipartClientRegistry.register(type.getPartType(colour, false), MultipartLightPartRenderer.INSTANCE);
                MultipartClientRegistry.register(type.getPartType(colour, true), MultipartLightPartRenderer.INSTANCE);

                // Item renderers
                MODEL_HELPER.register(
                        new ModelResourceLocation(Objects.requireNonNull(type.getItemRegistryObject(colour, false).getId()), "inventory"),
                        type.getProperties().getItemRenderer());
                MODEL_HELPER.register(
                        new ModelResourceLocation(Objects.requireNonNull(type.getItemRegistryObject(colour, true).getId()), "inventory"),
                        type.getProperties().getItemRenderer());
            }
        }
    }
}
