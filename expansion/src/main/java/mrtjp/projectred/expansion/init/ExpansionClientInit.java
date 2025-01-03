package mrtjp.projectred.expansion.init;

import codechicken.lib.render.block.BlockRenderingRegistry;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.expansion.GraphDebugManager;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.TubeType;
import mrtjp.projectred.expansion.client.*;
import mrtjp.projectred.expansion.gui.screen.inventory.*;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.expansion.tile.BatteryBoxBlockEntity;
import net.covers1624.quack.util.SneakyUtils;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.item.ItemProperties;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;
import static mrtjp.projectred.expansion.init.ExpansionItems.RECIPE_PLAN_ITEM;
import static mrtjp.projectred.expansion.init.ExpansionMenus.*;
import static mrtjp.projectred.expansion.init.ExpansionParts.FRAME_PART;

@SuppressWarnings("DataFlowIssue")
public class ExpansionClientInit {

    public static final ResourceLocation ITEM_MODEL_PROPERTY_CHARGE_LEVEL = new ResourceLocation(MOD_ID, "charge_level");
    public static final ResourceLocation ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN = new ResourceLocation(MOD_ID, "written");

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(ExpansionClientInit::clientSetup);

        // MovementManager hooks
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onRenderLevelStage);

        // GraphDebugManager hooks
        MinecraftForge.EVENT_BUS.addListener(GraphDebugManager::onRenderLevelStage);

        // Register sprites
        modEventBus.addListener(FrameModelRenderer::onTextureStitchEvent);
        modEventBus.addListener(FrameMotorBlockRenderer::onTextureStitchEvent);
        modEventBus.addListener(PneumaticSmokeParticle::onTextureStitchEvent);
        for (var type : TubeType.values()) {
            modEventBus.addListener(type::onTextureStitchEvent);
        }
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        MenuScreens.register(PROJECT_BENCH_MENU.get(), ProjectBenchScreen::new);
        MenuScreens.register(BATTERY_BOX_MENU.get(), BatteryBoxScreen::new);
        MenuScreens.register(AUTO_CRAFTER_MENU.get(), AutoCrafterScreen::new);
        MenuScreens.register(CHARGING_BENCH_MENU.get(), ChargingBenchScreen::new);
        MenuScreens.register(DEPLOYER_MENU.get(), DeployerScreen::new);

        // Register item model properties
        addItemModelProperties();

        // Register block renderers
        ItemBlockRenderTypes.setRenderLayer(FRAME_BLOCK.get(), RenderType.cutout());
        BlockRenderingRegistry.registerRenderer(FRAME_BLOCK.get(), FrameBlockRenderer.INSTANCE);
        ItemBlockRenderTypes.setRenderLayer(FRAME_MOTOR_BLOCK.get(), RenderType.solid());
        BlockRenderingRegistry.registerRenderer(FRAME_MOTOR_BLOCK.get(), FrameMotorBlockRenderer.INSTANCE);
        BlockRenderingRegistry.registerGlobalRenderer(MovingBlockSuppressorRenderer.INSTANCE);

        // Register part renderers
        MultipartClientRegistry.register(FRAME_PART.get(), FramePartRenderer.INSTANCE);

        // Register pipe renderers
        for (TubeType type : TubeType.values()) {
            // Block renderer
            MultipartClientRegistry.register(type.getPartType(), SneakyUtils.unsafeCast(TubePartRenderer.INSTANCE));
        }
    }

    private static void addItemModelProperties() {
        ItemProperties.register(BATTERY_BOX_BLOCK.get().asItem(), ITEM_MODEL_PROPERTY_CHARGE_LEVEL, (stack, world, entity, seed) -> {
            if (stack.hasTag()) {
                return stack.getTag().getInt(BatteryBoxBlockEntity.TAG_KEY_CHARGE_LEVEL_STATE);
            }
            return 0.0F;
        });

        ItemProperties.register(RECIPE_PLAN_ITEM.get(), ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN, (stack, world, entity, seed) -> RecipePlanItem.hasRecipeInside(stack) ? 1.0F : 0.0F);
    }
}
