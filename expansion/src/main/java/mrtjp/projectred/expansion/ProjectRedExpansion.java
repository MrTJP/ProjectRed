package mrtjp.projectred.expansion;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.PartConverter;
import codechicken.multipart.util.MultipartGenerator;
import mrtjp.projectred.api.Frame;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.expansion.data.*;
import mrtjp.projectred.expansion.init.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedExpansion {

    public static final String MOD_ID = "projectred_expansion";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(BuiltInRegistries.BLOCK, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, MOD_ID);
    public static final DeferredRegister<SoundEvent> SOUNDS = DeferredRegister.create(BuiltInRegistries.SOUND_EVENT, MOD_ID);
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(BuiltInRegistries.MENU, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<PartConverter> PART_CONVERTERS = DeferredRegister.create(PartConverter.PART_CONVERTERS, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    private static @Nullable ModContainer container;

    static {
        ProjectRedAPI.expansionAPI = ExpansionAPI.INSTANCE;

        ExpansionBlocks.register();
        ExpansionMenus.register();
        ExpansionItems.register();
        ExpansionSounds.register();
        ExpansionParts.register();
        ExpansionCreativeModeTabs.register();
    }

    public ProjectRedExpansion(ModContainer container, IEventBus modEventBus) {
        ProjectRedExpansion.container = container;

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);
        modEventBus.addListener(this::onRegisterCaps);

        if (FMLEnvironment.dist.isClient()) {
            ExpansionClientInit.init(modEventBus);
        }

        // Init packet handler
        ExpansionNetwork.init(modEventBus);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        SOUNDS.register(modEventBus);
        BLOCK_ENTITY_TYPES.register(modEventBus);
        MENU_TYPES.register(modEventBus);
        PART_TYPES.register(modEventBus);
        PART_CONVERTERS.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);

        // MovementManager hooks
        NeoForge.EVENT_BUS.addListener(MovementManager::onChunkWatchEvent);
        NeoForge.EVENT_BUS.addListener(MovementManager::onChunkUnwatchEvent);
        NeoForge.EVENT_BUS.addListener(MovementManager::onChunkUnloadEvent);
        NeoForge.EVENT_BUS.addListener(MovementManager::onLevelTick);
        NeoForge.EVENT_BUS.addListener(MovementManager::onLevelLoad);
        NeoForge.EVENT_BUS.addListener(MovementManager::onLevelUnload);

        // GraphDebugManager hooks
        NeoForge.EVENT_BUS.addListener(GraphDebugManager::onLevelUnload);
        NeoForge.EVENT_BUS.addListener(GraphDebugManager::onLevelTick);
        NeoForge.EVENT_BUS.addListener(GraphDebugManager::registerClientCommands);
    }

    public static ModContainer getContainer() {
        return Objects.requireNonNull(container);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        // Register frame as Multipart tile passthrough interface
        MultipartGenerator.INSTANCE.registerPassThroughInterface(Frame.class);

        // Init Movement registry
        MovementRegistry.init();
    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        generator.addProvider(event.includeClient(), new ExpansionBlockStateModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new ExpansionItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new ExpansionLanguageProvider(output));
        generator.addProvider(event.includeClient(), new ExpansionSoundProvider(output, fileHelper));

        generator.addProvider(event.includeServer(), new ExpansionBlockTagsProvider(output, event.getLookupProvider(), fileHelper));
        generator.addProvider(event.includeServer(), new ExpansionRecipeProvider(output));
        generator.addProvider(event.includeServer(), new ExpansionLootTableProvider(output));
    }

    public void onRegisterCaps(RegisterCapabilitiesEvent event) {
        ExpansionBlocks.registerCaps(event);
    }
}
