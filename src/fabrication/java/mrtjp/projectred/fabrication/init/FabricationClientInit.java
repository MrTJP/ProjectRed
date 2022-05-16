package mrtjp.projectred.fabrication.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.texture.SpriteRegistryHelper;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import mrtjp.projectred.integration.GateItemRenderer$;
import net.minecraft.client.renderer.model.ModelResourceLocation;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

public class FabricationClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(FabricationClientInit::clientSetup);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register sprites
        SpriteRegistryHelper iconRegister = new SpriteRegistryHelper();
        iconRegister.addIIconRegister(ICRenderTypes::registerIcons);

        // Register models
        ModelRegistryHelper modelRegistryHelper = new ModelRegistryHelper();
        modelRegistryHelper.register(new ModelResourceLocation(FabricationReferences.FABRICATED_GATE_ITEM.getRegistryName(), "inventory"), GateItemRenderer$.MODULE$);
    }
}
