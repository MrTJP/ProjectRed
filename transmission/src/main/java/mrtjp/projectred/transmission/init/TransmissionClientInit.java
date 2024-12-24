package mrtjp.projectred.transmission.init;

import codechicken.microblock.client.MicroMaterialClientRegistry;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.client.CenterWirePartRenderer;
import mrtjp.projectred.transmission.client.FaceWirePartRenderer;
import mrtjp.projectred.transmission.client.FramedWireHighlightRenderer;
import net.covers1624.quack.util.SneakyUtils;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;

public class TransmissionClientInit {

    public static void init(IEventBus modEventBus) {
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
