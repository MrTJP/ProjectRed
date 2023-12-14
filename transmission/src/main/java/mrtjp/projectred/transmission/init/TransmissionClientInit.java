package mrtjp.projectred.transmission.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.microblock.client.MicroMaterialClientRegistry;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.client.*;
import net.covers1624.quack.util.SneakyUtils;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import java.util.Objects;

public class TransmissionClientInit {

    private static final ModelRegistryHelper MODEL_HELPER = new ModelRegistryHelper();

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(TransmissionClientInit::clientSetup);

        // Register sprites
        SpriteRegistryHelper spriteHelper = new SpriteRegistryHelper(modEventBus);
        for (WireType type : WireType.values()) {
            type.registerTextures(spriteHelper);
        }
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Wire renderer registration
        for (WireType type : WireType.values()) {
            // Block renderer
            MultipartClientRegistry.register(type.getPartType(),
                    SneakyUtils.unsafeCast(type.isCenterPart() ? CenterWirePartRenderer.INSTANCE : FaceWirePartRenderer.INSTANCE));

            // Item renderer
            MODEL_HELPER.register(new ModelResourceLocation(Objects.requireNonNull(type.getItemRegistryObject().getId()), "inventory"),
                    type.isCenterPart() ? FramedWirePartItemRenderer.INSTANCE : WirePartItemRenderer.INSTANCE);
        }

        // Highlight renderer for cover-on-wire overlay
        MicroMaterialClientRegistry.registerGlobalHighlightRenderer(FramedWireHighlightRenderer.INSTANCE);
    }
}
