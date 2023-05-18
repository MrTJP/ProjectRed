package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.gui.screen.inventory.AutoCrafterScreen;
import mrtjp.projectred.expansion.gui.screen.inventory.BatteryBoxScreen;
import mrtjp.projectred.expansion.gui.screen.inventory.ChargingBenchScreen;
import mrtjp.projectred.expansion.gui.screen.inventory.ProjectBenchScreen;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.expansion.tile.BatteryBoxTile;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraft.client.renderer.item.ItemProperties;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

public class ExpansionClientInit {

    public static final ResourceLocation ITEM_MODEL_PROPERTY_CHARGE_LEVEL = new ResourceLocation(MOD_ID, "charge_level");
    public static final ResourceLocation ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN = new ResourceLocation(MOD_ID, "written");

    public static void init() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(ExpansionClientInit::clientSetup);
    }

    private static void clientSetup(final FMLClientSetupEvent event) {

        // Register screens
        MenuScreens.register(PROJECT_BENCH_CONTAINER, ProjectBenchScreen::new);
        MenuScreens.register(BATTERY_BOX_CONTAINER, BatteryBoxScreen::new);
        MenuScreens.register(AUTO_CRAFTER_CONTAINER, AutoCrafterScreen::new);
        MenuScreens.register(CHARGING_BENCH_CONTAINER, ChargingBenchScreen::new);

        // Register item model properties
        addItemModelProperties();
    }

    private static void addItemModelProperties() {
        ItemProperties.register(BATTERY_BOX_BLOCK.asItem(), ITEM_MODEL_PROPERTY_CHARGE_LEVEL, (stack, world, entity, seed) -> {
            if (stack.hasTag()) {
                return stack.getTag().getInt(BatteryBoxTile.TAG_KEY_CHARGE_LEVEL_STATE);
            }
            return 0.0F;
        });

        ItemProperties.register(RECIPE_PLAN_ITEM, ITEM_MODEL_PROPERTY_WRITTEN_RECIPE_PLAN, (stack, world, entity, seed) -> RecipePlanItem.hasRecipeInside(stack) ? 1.0F : 0.0F);
    }
}
