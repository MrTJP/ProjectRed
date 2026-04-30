package mrtjp.projectred.expansion.init;

import codechicken.multipart.api.MultipartClientRegistry;
import codechicken.multipart.api.part.render.PartBakedModelRenderer;
import mrtjp.projectred.expansion.GraphDebugManager;
import mrtjp.projectred.expansion.TubeType;
import mrtjp.projectred.expansion.client.*;
import mrtjp.projectred.expansion.compatibility.EmbeddiumCompatibility;
import mrtjp.projectred.expansion.gui.screen.inventory.*;
import mrtjp.projectred.expansion.item.BatteryBoxStorageComponent;
import mrtjp.projectred.expansion.item.RecipePlanComponent;
import net.covers1624.quack.util.SneakyUtils;
import net.minecraft.client.renderer.item.ItemProperties;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModList;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.neoforge.client.event.RegisterMenuScreensEvent;
import net.neoforged.neoforge.common.NeoForge;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.BATTERY_BOX_BLOCK;
import static mrtjp.projectred.expansion.init.ExpansionItems.RECIPE_PLAN_ITEM;
import static mrtjp.projectred.expansion.init.ExpansionMenus.*;
import static mrtjp.projectred.expansion.init.ExpansionParts.FRAME_PART;

public class ExpansionClientInit {

    public static final ResourceLocation ITEM_MODEL_PROPERTY_CHARGE_LEVEL = ResourceLocation.fromNamespaceAndPath(MOD_ID, "charge_level");
    public static final ResourceLocation ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN = ResourceLocation.fromNamespaceAndPath(MOD_ID, "written");

    public static void init(IEventBus modEventBus) {
        modEventBus.addListener(ExpansionClientInit::clientSetup);

        // Register menu screens
        modEventBus.addListener(ExpansionClientInit::onRegisterMenuScreensEvent);

        // MovementManager hooks
        NeoForge.EVENT_BUS.addListener(MovingBlockRenderManager::onRenderLevelStage);
        NeoForge.EVENT_BUS.addListener(MovingBlockRenderManager::onDrawHighlight);

        // GraphDebugManager hooks
        NeoForge.EVENT_BUS.addListener(GraphDebugManager::onRenderLevelStage);

        // Register sprites
        modEventBus.addListener(FrameModelRenderer::onTextureStitchEvent);
        modEventBus.addListener(FrameMotorBlockModel::onTextureStitchEvent);
        modEventBus.addListener(PneumaticSmokeParticle::onTextureStitchEvent);
        for (var type : TubeType.values()) {
            modEventBus.addListener(type::onTextureStitchEvent);
        }
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register item model properties
        addItemModelProperties();

        // Register part renderers
        MultipartClientRegistry.register(FRAME_PART.get(), PartBakedModelRenderer.simple());

        // Register pipe renderers
        for (TubeType type : TubeType.values()) {
            // Block renderer
            MultipartClientRegistry.register(type.getPartType(), SneakyUtils.unsafeCast(TubePartRenderer.INSTANCE));
        }

        // Load compatibility modules
        //noinspection Convert2MethodRef
        ModList.get().getModContainerById("embeddium")
                .ifPresent(mod -> EmbeddiumCompatibility.initClient(mod));
    }

    private static void onRegisterMenuScreensEvent(RegisterMenuScreensEvent event) {
        event.register(PROJECT_BENCH_MENU.get(), ProjectBenchScreen::new);
        event.register(BATTERY_BOX_MENU.get(), BatteryBoxScreen::new);
        event.register(AUTO_CRAFTER_MENU.get(), AutoCrafterScreen::new);
        event.register(CHARGING_BENCH_MENU.get(), ChargingBenchScreen::new);
        event.register(DEPLOYER_MENU.get(), DeployerScreen::new);
    }

    private static void addItemModelProperties() {
        ItemProperties.register(BATTERY_BOX_BLOCK.get().asItem(), ITEM_MODEL_PROPERTY_CHARGE_LEVEL, (stack, world, entity, seed) -> {
            var component = BatteryBoxStorageComponent.getComponent(stack);
            return component != null ? component.storedPowerRenderLevel() : 0.0F;
        });

        ItemProperties.register(RECIPE_PLAN_ITEM.get(), ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN, (stack, world, entity, seed) -> {
            var recipeData = RecipePlanComponent.getComponent(stack);
            return recipeData != null && recipeData.isRecipeValid() ? 1.0F : 0.0F;
        });
    }
}
