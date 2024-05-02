package mrtjp.projectred.expansion;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.PartConverter;
import codechicken.multipart.util.MultipartGenerator;
import mrtjp.projectred.api.Frame;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.expansion.data.*;
import mrtjp.projectred.expansion.init.*;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.data.event.GatherDataEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedExpansion {

    public static final String MOD_ID = "projectred_expansion";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<SoundEvent> SOUNDS = DeferredRegister.create(ForgeRegistries.SOUND_EVENTS, MOD_ID);
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(ForgeRegistries.BLOCK_ENTITY_TYPES, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(ForgeRegistries.MENU_TYPES, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<PartConverter> PART_CONVERTERS = DeferredRegister.create(PartConverter.PART_CONVERTERS, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    static {
        ProjectRedAPI.expansionAPI = ExpansionAPI.INSTANCE;

        ExpansionBlocks.register();
        ExpansionMenus.register();
        ExpansionItems.register();
        ExpansionSounds.register();
        ExpansionParts.register();
        ExpansionCreativeModeTabs.register();
    }

    public ProjectRedExpansion() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> ExpansionClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        SOUNDS.register(modEventBus);
        BLOCK_ENTITY_TYPES.register(modEventBus);
        MENU_TYPES.register(modEventBus);
        PART_TYPES.register(modEventBus);
        PART_CONVERTERS.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);

        // MovementManager hooks
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onChunkWatchEvent);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onChunkUnwatchEvent);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onChunkUnloadEvent);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onLevelTick);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onLevelLoad);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onLevelUnload);

        // GraphDebugManager hooks
        MinecraftForge.EVENT_BUS.addListener(GraphDebugManager::onLevelUnload);
        MinecraftForge.EVENT_BUS.addListener(GraphDebugManager::onLevelTick);
        MinecraftForge.EVENT_BUS.addListener(GraphDebugManager::registerClientCommands);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        // Init packet handler
        ExpansionNetwork.init();

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
}
