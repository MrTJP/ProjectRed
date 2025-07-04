package mrtjp.projectred.exploration;

import codechicken.microblock.api.MicroMaterial;
import com.mojang.serialization.MapCodec;
import mrtjp.projectred.exploration.data.*;
import mrtjp.projectred.exploration.init.*;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.ArmorMaterial;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.levelgen.carver.WorldCarver;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.common.world.BiomeModifier;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import net.neoforged.neoforge.registries.NeoForgeRegistries;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedExploration {

    public static final String MOD_ID = "projectred_exploration";

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(BuiltInRegistries.BLOCK, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(BuiltInRegistries.MENU, MOD_ID);
    public static final DeferredRegister<WorldCarver<?>> WORLD_CARVERS = DeferredRegister.create(BuiltInRegistries.CARVER, MOD_ID);
    public static final DeferredRegister<MapCodec<? extends BiomeModifier>> BIOME_MODIFIER_SERIALIZERS = DeferredRegister.create(NeoForgeRegistries.BIOME_MODIFIER_SERIALIZERS, MOD_ID);
    public static final DeferredRegister<RecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(BuiltInRegistries.RECIPE_SERIALIZER, MOD_ID);
    public static final DeferredRegister<MicroMaterial> MICRO_MATERIALS = DeferredRegister.create(MicroMaterial.REGISTRY, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);
    public static final DeferredRegister<DataComponentType<?>> DATA_COMPONENT_TYPES = DeferredRegister.create(BuiltInRegistries.DATA_COMPONENT_TYPE, MOD_ID);
    public static final DeferredRegister<ArmorMaterial> ARMOR_MATERIALS = DeferredRegister.create(BuiltInRegistries.ARMOR_MATERIAL, MOD_ID);

    private static @Nullable ModContainer container;

    static {
        ExplorationBlocks.register();
        ExplorationItems.register();
        ExplorationMenus.register();
        ExplorationWorldFeatures.register();
        ExplorationRecipeSerializers.register();
        ExplorationCreativeModeTabs.register();
        ExplorationDataComponents.register();
        ExplorationArmorMaterials.register();
    }

    public ProjectRedExploration(ModContainer container, IEventBus modEventBus) {
        ProjectRedExploration.container = container;

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);
        modEventBus.addListener(ExplorationBlocks::onRegisterMicroMaterials);

        if (FMLEnvironment.dist.isClient()) {
            ExplorationClientInit.init(modEventBus);
        }

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        MENU_TYPES.register(modEventBus);
        WORLD_CARVERS.register(modEventBus);
        BIOME_MODIFIER_SERIALIZERS.register(modEventBus);
        RECIPE_SERIALIZERS.register(modEventBus);
        MICRO_MATERIALS.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);
        DATA_COMPONENT_TYPES.register(modEventBus);
        ARMOR_MATERIALS.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();
        CompletableFuture<HolderLookup.Provider> provider = event.getLookupProvider();

        generator.addProvider(event.includeClient(), new ExplorationBlockStateModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new ExplorationItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new ExplorationLanguageProvider(output));

        ExplorationBuiltInEntriesProvider explorationProvider = new ExplorationBuiltInEntriesProvider(output, provider);
        generator.addProvider(event.includeServer(), explorationProvider);
        BlockTagsProvider blockTagsProvider = new ExplorationBlockTagsProvider(output, provider, fileHelper);
        generator.addProvider(event.includeServer(), blockTagsProvider);
        generator.addProvider(event.includeServer(), new ExplorationItemTagsProvider(output, provider, blockTagsProvider.contentsGetter(), fileHelper));
        generator.addProvider(event.includeServer(), new ExplorationLootTableProvider(output, provider));
        generator.addProvider(event.includeServer(), new ExplorationRecipeProvider(provider, output));
    }
}
