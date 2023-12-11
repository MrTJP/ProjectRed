package mrtjp.projectred.exploration;

import codechicken.lib.gui.SimpleCreativeTab;
import codechicken.microblock.CBMicroblock;
import codechicken.microblock.api.MicroMaterial;
import mrtjp.projectred.exploration.data.*;
import mrtjp.projectred.exploration.init.*;
import net.minecraft.core.Registry;
import net.minecraft.data.DataGenerator;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.levelgen.carver.ConfiguredWorldCarver;
import net.minecraft.world.level.levelgen.carver.WorldCarver;
import net.minecraft.world.level.levelgen.feature.ConfiguredFeature;
import net.minecraft.world.level.levelgen.feature.Feature;
import net.minecraft.world.level.levelgen.placement.PlacedFeature;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.data.event.GatherDataEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.MARBLE_BRICK_BLOCK;

@Mod(MOD_ID)
public class ProjectRedExploration {

    public static final String MOD_ID = "projectred_exploration";

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(ForgeRegistries.MENU_TYPES, MOD_ID);
    public static final DeferredRegister<WorldCarver<?>> WORLD_CARVERS = DeferredRegister.create(ForgeRegistries.WORLD_CARVERS, MOD_ID);
    public static final DeferredRegister<ConfiguredWorldCarver<?>> CONFIGURED_CARVERS = DeferredRegister.create(Registry.CONFIGURED_CARVER_REGISTRY, MOD_ID);
    public static final DeferredRegister<Feature<?>> FEATURES = DeferredRegister.create(ForgeRegistries.FEATURES, MOD_ID);
    public static final DeferredRegister<ConfiguredFeature<?, ?>> CONFIGURED_FEATURES = DeferredRegister.create(Registry.CONFIGURED_FEATURE_REGISTRY, MOD_ID);
    public static final DeferredRegister<PlacedFeature> PLACED_FEATURES = DeferredRegister.create(Registry.PLACED_FEATURE_REGISTRY, MOD_ID);
    public static final DeferredRegister<RecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(ForgeRegistries.RECIPE_SERIALIZERS, MOD_ID);
    public static final DeferredRegister<MicroMaterial> MICRO_MATERIALS = DeferredRegister.create(new ResourceLocation(CBMicroblock.MOD_ID, "micro_material"), MOD_ID);

    public static final SimpleCreativeTab EXPLORATION_CREATIVE_TAB = new SimpleCreativeTab(MOD_ID, () -> new ItemStack(MARBLE_BRICK_BLOCK.get()));

    static {
        ExplorationBlocks.register();
        ExplorationItems.register();
        ExplorationMenus.register();
        ExplorationWorldFeatures.register();
        ExplorationRecipeSerializers.register();
    }

    public ProjectRedExploration() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);
        modEventBus.addListener(ExplorationBlocks::onRegisterMicroMaterials);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> ExplorationClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        MENU_TYPES.register(modEventBus);
        WORLD_CARVERS.register(modEventBus);
        CONFIGURED_CARVERS.register(modEventBus);
        FEATURES.register(modEventBus);
        CONFIGURED_FEATURES.register(modEventBus);
        PLACED_FEATURES.register(modEventBus);
        RECIPE_SERIALIZERS.register(modEventBus);
        MICRO_MATERIALS.register(modEventBus);

        //TODO figure out how to load in features
//        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGH, ExplorationWorldFeatures::onBiomeLoadingEvent);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

        //TODO figure out how to load in features
//        event.enqueueWork(ExplorationWorldFeatures::load);
    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        generator.addProvider(event.includeClient(), new ExplorationBlockStateModelProvider(generator, fileHelper));
        generator.addProvider(event.includeClient(), new ExplorationItemModelProvider(generator, fileHelper));
        generator.addProvider(event.includeClient(), new ExplorationLanguageProvider(generator));

        generator.addProvider(event.includeServer(), new ExplorationBlockTagsProvider(generator, fileHelper));
        generator.addProvider(event.includeServer(), new ExplorationItemTagsProvider(generator, fileHelper));
        generator.addProvider(event.includeServer(), new ExplorationLootTableProvider(generator));
        generator.addProvider(event.includeServer(), new ExplorationRecipeProvider(generator));
    }
}
