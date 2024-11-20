package mrtjp.projectred.transmission.init;

import codechicken.microblock.client.MicroMaterialClientRegistry;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.client.CenterWirePartRenderer;
import mrtjp.projectred.transmission.client.FaceWirePartRenderer;
import mrtjp.projectred.transmission.client.FramedWireHighlightRenderer;
import net.covers1624.quack.util.SneakyUtils;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

public class TransmissionClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(TransmissionClientInit::clientSetup);

        // Register sprites
        for (WireType type : WireType.values()) {
            modEventBus.addListener(type::onTextureStitchEvent);
        }
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Wire part renderer registration
        for (WireType type : WireType.values()) {
            MultipartClientRegistry.register(type.getPartType(),
                    SneakyUtils.unsafeCast(type.isCenterPart() ? CenterWirePartRenderer.INSTANCE : FaceWirePartRenderer.INSTANCE));
        }

        // Highlight renderer for cover-on-wire overlay
        MicroMaterialClientRegistry.registerGlobalHighlightRenderer(FramedWireHighlightRenderer.INSTANCE);
    }
}
