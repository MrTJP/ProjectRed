package mrtjp.projectred.illumination.init;

import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import mrtjp.projectred.illumination.client.*;
import net.covers1624.quack.util.SneakyUtils;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderers;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.client.event.ModelEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.Objects;

public class IlluminationClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(IlluminationClientInit::clientSetup);
        modEventBus.addListener(IlluminationClientInit::onModelBake);

        // Register Sprites
        for (MultipartLightType type : MultipartLightType.values()) {
            modEventBus.addListener(type.getProperties()::onTextureStitchEvent);
        }
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

        BlockEntityRenderers.register(SneakyUtils.unsafeCast(IlluminationBlocks.ILLUMAR_SMART_LAMP_BLOCK_ENTITY.get()), c -> IllumarSmartLampBlockEntityRenderer.INSTANCE);

        // Register light part renderers
        for (MultipartLightType type : MultipartLightType.values()) {
            for (int colour = 0; colour < 16; colour++) {
                MultipartClientRegistry.register(type.getPartType(colour, false), MultipartLightPartRenderer.INSTANCE);
                MultipartClientRegistry.register(type.getPartType(colour, true), MultipartLightPartRenderer.INSTANCE);
            }
        }
    }

    public static void onModelBake(ModelEvent.ModifyBakingResult event) {

        // Replace item models for inverted lamps with a wrapped renderer that renders the lamp glow
        for (int color = 0; color < 16; color++) {
            ResourceLocation blockRL = Objects.requireNonNull(ForgeRegistries.BLOCKS.getKey(BlockLightType.ILLUMAR_LAMP.getBlock(color, true)));
            // Override default BlockItem renderer for the lit variants to render lamp glow
            BakedModel litModel = event.getModels().get(new ModelResourceLocation(blockRL, "lit=true"));
            event.getModels().put(
                    new ModelResourceLocation(blockRL, "inventory"),
                    new IllumarLampItemRenderer(litModel));
        }

        // Illumar smart lamp renderer
        ResourceLocation smartLampRl = Objects.requireNonNull(ForgeRegistries.BLOCKS.getKey(IlluminationBlocks.ILLUMAR_SMART_LAMP.get()));
        BakedModel smartLampModel = event.getModels().get(new ModelResourceLocation(smartLampRl, "level=15,side=0"));
        event.getModels().put(
                new ModelResourceLocation(smartLampRl, "inventory"),
                new IllumarSmartLampItemRenderer(smartLampModel));
    }
}
