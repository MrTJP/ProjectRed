package mrtjp.projectred.core.init;

import codechicken.lib.render.shader.CCShaderInstance;
import codechicken.lib.util.ResourceUtils;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.core.gui.screen.inventory.ElectrotineGeneratorScreen;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.neoforge.client.event.RegisterShadersEvent;
import net.neoforged.neoforge.common.NeoForge;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreMenus.ELECTROTINE_GENERATOR_MENU;

@SuppressWarnings("NotNullFieldNotInitialized")
public class CoreClientInit {

    public static CCShaderInstance HALO_SHADER;

    public static void init(IEventBus modEventBus) {
        modEventBus.addListener(CoreClientInit::clientSetup);
        modEventBus.addListener(CoreClientInit::onRegisterShaders);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        MenuScreens.register(ELECTROTINE_GENERATOR_MENU.get(), ElectrotineGeneratorScreen::new);

        // Init Halo Renderer
        HaloRenderer.init();

        // Register Halo renderer
        NeoForge.EVENT_BUS.addListener(HaloRenderer::onRenderLevelStageEvent);

        // Register resource reload listener
        ResourceUtils.registerReloadListener(HaloRenderer::onResourceManagerReload);
    }

    private static void onRegisterShaders(RegisterShadersEvent event) {
        event.registerShader(CCShaderInstance.create(event.getResourceProvider(), new ResourceLocation(MOD_ID, "halo"), DefaultVertexFormat.POSITION_COLOR), e -> {
            HALO_SHADER = (CCShaderInstance) e;
        });
    }
}
