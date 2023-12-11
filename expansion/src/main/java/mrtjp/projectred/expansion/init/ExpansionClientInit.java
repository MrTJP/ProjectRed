package mrtjp.projectred.expansion.init;

import codechicken.lib.model.ModelRegistryHelper;
import codechicken.lib.render.block.BlockRenderingRegistry;
import codechicken.lib.texture.SpriteRegistryHelper;
import codechicken.multipart.api.MultipartClientRegistry;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.client.*;
import mrtjp.projectred.expansion.gui.screen.inventory.AutoCrafterScreen;
import mrtjp.projectred.expansion.gui.screen.inventory.BatteryBoxScreen;
import mrtjp.projectred.expansion.gui.screen.inventory.ChargingBenchScreen;
import mrtjp.projectred.expansion.gui.screen.inventory.ProjectBenchScreen;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.expansion.tile.BatteryBoxTile;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.item.ItemProperties;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;
import static mrtjp.projectred.expansion.init.ExpansionItems.RECIPE_PLAN_ITEM;
import static mrtjp.projectred.expansion.init.ExpansionMenus.*;
import static mrtjp.projectred.expansion.init.ExpansionParts.FRAME_PART;

@SuppressWarnings("DataFlowIssue")
public class ExpansionClientInit {

    public static final ResourceLocation ITEM_MODEL_PROPERTY_CHARGE_LEVEL = new ResourceLocation(MOD_ID, "charge_level");
    public static final ResourceLocation ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN = new ResourceLocation(MOD_ID, "written");

    private static final ModelRegistryHelper MODEL_HELPER = new ModelRegistryHelper();

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(ExpansionClientInit::clientSetup);

        MinecraftForge.EVENT_BUS.addListener(MovementManager::onRenderLevelStage);

        // Register sprites
        SpriteRegistryHelper spriteHelper = new SpriteRegistryHelper(modEventBus);
        spriteHelper.addIIconRegister(FrameModelRenderer::registerIcons);
        spriteHelper.addIIconRegister(FrameMotorBlockRenderer::registerIcons);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        MenuScreens.register(PROJECT_BENCH_CONTAINER.get(), ProjectBenchScreen::new);
        MenuScreens.register(BATTERY_BOX_CONTAINER.get(), BatteryBoxScreen::new);
        MenuScreens.register(AUTO_CRAFTER_CONTAINER.get(), AutoCrafterScreen::new);
        MenuScreens.register(CHARGING_BENCH_CONTAINER.get(), ChargingBenchScreen::new);

        // Register item model properties
        addItemModelProperties();

        // Register block renderers
        ItemBlockRenderTypes.setRenderLayer(FRAME_BLOCK.get(), RenderType.cutout());
        BlockRenderingRegistry.registerRenderer(FRAME_BLOCK.get(), FrameBlockRenderer.INSTANCE);
        ItemBlockRenderTypes.setRenderLayer(FRAME_MOTOR_BLOCK.get(), RenderType.solid());
        BlockRenderingRegistry.registerRenderer(FRAME_MOTOR_BLOCK.get(), FrameMotorBlockRenderer.INSTANCE);
        BlockRenderingRegistry.registerGlobalRenderer(MovingBlockSuppressorRenderer.INSTANCE);

        // Register item renderers
        MODEL_HELPER.register(new ModelResourceLocation(ForgeRegistries.BLOCKS.getKey(FRAME_BLOCK.get()), "inventory"), FrameBlockRenderer.INSTANCE);
        MODEL_HELPER.register(new ModelResourceLocation(ForgeRegistries.BLOCKS.getKey(FRAME_MOTOR_BLOCK.get()), "inventory"), FrameMotorBlockRenderer.INSTANCE);

        // Register part renderers
        MultipartClientRegistry.register(FRAME_PART.get(), FramePartRenderer.INSTANCE);
    }

    private static void addItemModelProperties() {
        ItemProperties.register(BATTERY_BOX_BLOCK.get().asItem(), ITEM_MODEL_PROPERTY_CHARGE_LEVEL, (stack, world, entity, seed) -> {
            if (stack.hasTag()) {
                return stack.getTag().getInt(BatteryBoxTile.TAG_KEY_CHARGE_LEVEL_STATE);
            }
            return 0.0F;
        });

        ItemProperties.register(RECIPE_PLAN_ITEM.get(), ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN, (stack, world, entity, seed) -> RecipePlanItem.hasRecipeInside(stack) ? 1.0F : 0.0F);
    }
}
