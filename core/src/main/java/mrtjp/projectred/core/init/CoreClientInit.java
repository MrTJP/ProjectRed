package mrtjp.projectred.core.init;

import codechicken.lib.render.shader.CCShaderInstance;
import codechicken.lib.util.ResourceUtils;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.core.gui.screen.inventory.ElectrotineGeneratorScreen;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.client.event.RegisterShadersEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreMenus.ELECTROTINE_GENERATOR_CONTAINER;

@SuppressWarnings("NotNullFieldNotInitialized")
public class CoreClientInit {

    public static CCShaderInstance HALO_SHADER;

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(CoreClientInit::clientSetup);
        modEventBus.addListener(CoreClientInit::onRegisterShaders);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        MenuScreens.register(ELECTROTINE_GENERATOR_CONTAINER.get(), ElectrotineGeneratorScreen::new);

        // Init Halo Renderer
        HaloRenderer.init();

        // Register Halo renderer
        MinecraftForge.EVENT_BUS.addListener(HaloRenderer::onRenderLevelStageEvent);

        // Register resource reload listener
        ResourceUtils.registerReloadListener(HaloRenderer::onResourceManagerReload);
    }

    private static void onRegisterShaders(RegisterShadersEvent event) {
        event.registerShader(CCShaderInstance.create(event.getResourceProvider(), new ResourceLocation(MOD_ID, "halo"), DefaultVertexFormat.POSITION_COLOR), e -> {
            HALO_SHADER = (CCShaderInstance) e;
        });
    }
}
