package mrtjp.projectred.exploration;

import codechicken.microblock.CBMicroblock;
import codechicken.microblock.api.MicroMaterial;
import com.mojang.serialization.Codec;
import mrtjp.projectred.exploration.data.*;
import mrtjp.projectred.exploration.init.*;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.levelgen.carver.WorldCarver;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.data.BlockTagsProvider;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.common.world.BiomeModifier;
import net.minecraftforge.data.event.GatherDataEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedExploration {

    public static final String MOD_ID = "projectred_exploration";

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(ForgeRegistries.MENU_TYPES, MOD_ID);
    public static final DeferredRegister<WorldCarver<?>> WORLD_CARVERS = DeferredRegister.create(ForgeRegistries.WORLD_CARVERS, MOD_ID);
    public static final DeferredRegister<Codec<? extends BiomeModifier>> BIOME_MODIFIER_SERIALIZERS = DeferredRegister.create(ForgeRegistries.Keys.BIOME_MODIFIER_SERIALIZERS, MOD_ID);
    public static final DeferredRegister<RecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(ForgeRegistries.RECIPE_SERIALIZERS, MOD_ID);
    public static final DeferredRegister<MicroMaterial> MICRO_MATERIALS = DeferredRegister.create(new ResourceLocation(CBMicroblock.MOD_ID, "micro_material"), MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    static {
        ExplorationBlocks.register();
        ExplorationItems.register();
        ExplorationMenus.register();
        ExplorationWorldFeatures.register();
        ExplorationRecipeSerializers.register();
        ExplorationCreativeModeTabs.register();
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
        BIOME_MODIFIER_SERIALIZERS.register(modEventBus);
        RECIPE_SERIALIZERS.register(modEventBus);
        MICRO_MATERIALS.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        generator.addProvider(event.includeClient(), new ExplorationBlockStateModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new ExplorationItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new ExplorationLanguageProvider(output));

        ExplorationBuiltInEntriesProvider explorationProvider = new ExplorationBuiltInEntriesProvider(output, event.getLookupProvider());
        generator.addProvider(event.includeServer(), explorationProvider);
        BlockTagsProvider blockTagsProvider = new ExplorationBlockTagsProvider(output, event.getLookupProvider(), fileHelper);
        generator.addProvider(event.includeServer(), blockTagsProvider);
        generator.addProvider(event.includeServer(), new ExplorationItemTagsProvider(output, event.getLookupProvider(), blockTagsProvider.contentsGetter(), fileHelper));
        generator.addProvider(event.includeServer(), new ExplorationLootTableProvider(output));
        generator.addProvider(event.includeServer(), new ExplorationRecipeProvider(output));
    }
}
