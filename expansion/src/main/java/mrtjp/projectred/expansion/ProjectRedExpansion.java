package mrtjp.projectred.expansion;

import codechicken.lib.gui.SimpleCreativeTab;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.PartConverter;
import codechicken.multipart.util.MultipartGenerator;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.expansion.data.*;
import mrtjp.projectred.expansion.init.*;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.forge.event.lifecycle.GatherDataEvent;
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
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITIES = DeferredRegister.create(ForgeRegistries.BLOCK_ENTITIES, MOD_ID);
    public static final DeferredRegister<MenuType<?>> CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PARTS = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<PartConverter> PART_CONVERTERS = DeferredRegister.create(PartConverter.PART_CONVERTERS, MOD_ID);

    public static final SimpleCreativeTab EXPANSION_GROUP = new SimpleCreativeTab(MOD_ID, () -> new ItemStack(ExpansionReferences.PROJECT_BENCH_BLOCK));

    static {
        ProjectRedAPI.expansionAPI = ExpansionAPI.INSTANCE;

        ExpansionBlocks.register();
        ExpansionContainers.register();
        ExpansionItems.register();
        ExpansionParts.register();
    }

    public ProjectRedExpansion() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.safeRunWhenOn(Dist.CLIENT, () -> ExpansionClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        BLOCK_ENTITIES.register(modEventBus);
        CONTAINERS.register(modEventBus);
        PARTS.register(modEventBus);
        PART_CONVERTERS.register(modEventBus);

        MinecraftForge.EVENT_BUS.addListener(MovementManager::onChunkWatchEvent);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onChunkUnwatchEvent);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onChunkUnloadEvent);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onLevelTick);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onLevelLoad);
        MinecraftForge.EVENT_BUS.addListener(MovementManager::onLevelUnload);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        // Init packet handler
        ExpansionNetwork.init();

        // Register frame as Multipart tile passthrough interface
        MultipartGenerator.INSTANCE.registerPassThroughInterface("mrtjp.projectred.api.Frame");

        // Init Movement registry
        MovementRegistry.init();
    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        if (event.includeClient()) {
            generator.addProvider(new ExpansionBlockStateModelProvider(generator, fileHelper));
            generator.addProvider(new ExpansionItemModelProvider(generator, fileHelper));
            generator.addProvider(new ExpansionLanguageProvider(generator));
        }
        if (event.includeServer()) {
            generator.addProvider(new ExpansionBlockTagsProvider(generator, fileHelper));
            generator.addProvider(new ExpansionRecipeProvider(generator));
            generator.addProvider(new ExpansionLootTableProvider(generator));
        }
    }
}
