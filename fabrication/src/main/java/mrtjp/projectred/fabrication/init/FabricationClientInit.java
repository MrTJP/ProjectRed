package mrtjp.projectred.fabrication.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import mrtjp.projectred.fabrication.gui.screen.inventory.LithographyTableScreen;
import mrtjp.projectred.fabrication.gui.screen.inventory.PackagingTableScreen;
import mrtjp.projectred.fabrication.gui.screen.inventory.PlottingTableScreen;
import mrtjp.projectred.integration.client.GatePartItemRenderer;
import mrtjp.projectred.integration.client.GatePartRenderer;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.fabrication.init.FabricationMenus.*;

@SuppressWarnings("DataFlowIssue")
public class FabricationClientInit {

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(FabricationClientInit::clientSetup);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register sprites
        SpriteRegistryHelper iconRegister = new SpriteRegistryHelper();
        iconRegister.addIIconRegister(ICRenderTypes::registerIcons);

        // Register part renderers
        MultipartClientRegistry.register(FabricationParts.FABRICATED_GATE_PART.get(), GatePartRenderer.INSTANCE);

        // Register models
        ModelRegistryHelper modelRegistryHelper = new ModelRegistryHelper();
        modelRegistryHelper.register(new ModelResourceLocation(ForgeRegistries.ITEMS.getKey(FabricationParts.FABRICATED_GATE_ITEM.get()), "inventory"), GatePartItemRenderer.INSTANCE);

        // Register screens
        MenuScreens.register(PLOTTING_TABLE_MENU.get(), PlottingTableScreen::new);
        MenuScreens.register(LITHOGRAPHY_TABLE_MENU.get(), LithographyTableScreen::new);
        MenuScreens.register(PACKAGING_TABLE_MENU.get(), PackagingTableScreen::new);
    }
}
