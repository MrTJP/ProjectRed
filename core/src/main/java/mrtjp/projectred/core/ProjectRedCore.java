package mrtjp.projectred.core;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.compatibility.ComputerCraftCompatibility;
import mrtjp.projectred.core.data.*;
import mrtjp.projectred.core.init.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.OptionalMod;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedCore {

    public static final String MOD_ID = "projectred_core";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(BuiltInRegistries.BLOCK, MOD_ID);
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(BuiltInRegistries.MENU, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, MOD_ID);
    public static final DeferredRegister<RecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(BuiltInRegistries.RECIPE_SERIALIZER, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    private static @Nullable ModContainer container;

    static {
        CoreBlocks.register();
        CoreItems.register();
        CoreMenus.register();
        CoreParts.register();
        CoreCreativeModeTabs.register();
    }

    public ProjectRedCore(ModContainer container, IEventBus modEventBus) {
        ProjectRedCore.container = container;

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);
        modEventBus.addListener(this::onRegisterCaps);

        if (FMLEnvironment.dist.isClient()) {
            CoreClientInit.init(modEventBus);
        }

        // Init packet handler
        CoreNetwork.init(modEventBus);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        BLOCK_ENTITY_TYPES.register(modEventBus);
        MENU_TYPES.register(modEventBus);
        PART_TYPES.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);
    }

    public static ModContainer getContainer() {
        return Objects.requireNonNull(container);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        // Load config file
        Configurator.load();

        // Load compatibility modules
        if (Configurator.compat_CCBundledCable) {
            //noinspection Convert2MethodRef
            OptionalMod.of("computercraft").ifPresent(mod -> ComputerCraftCompatibility.init(mod));
        }
    }

    private void onGatherDataEvent(final GatherDataEvent event) {

        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        generator.addProvider(event.includeClient(), new CoreBlockStateModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new CoreItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new CoreLanguageProvider(output));

        generator.addProvider(event.includeServer(), new CoreRecipeProvider(output));
        generator.addProvider(event.includeServer(), new CoreLootTableProvider(output));

        BlockTagsProvider blockTagsProvider = new CoreBlockTagsProvider(output, event.getLookupProvider(), fileHelper);
        generator.addProvider(event.includeServer(), blockTagsProvider);
        generator.addProvider(event.includeServer(), new CoreItemTagsProvider(output, event.getLookupProvider(), blockTagsProvider.contentsGetter(), fileHelper));
    }

    public void onRegisterCaps(RegisterCapabilitiesEvent event) {
        CoreBlocks.registerCaps(event);
    }
}
