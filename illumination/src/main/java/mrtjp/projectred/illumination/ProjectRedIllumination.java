package mrtjp.projectred.illumination;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.illumination.data.*;
import mrtjp.projectred.illumination.init.IlluminationBlocks;
import mrtjp.projectred.illumination.init.IlluminationClientInit;
import mrtjp.projectred.illumination.init.IlluminationMicroMaterials;
import mrtjp.projectred.illumination.init.IlluminationParts;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedIllumination {

    public static final String MOD_ID = "projectred_illumination";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(BuiltInRegistries.BLOCK, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, MOD_ID);
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    private static @Nullable ModContainer container;

    static {
        IlluminationBlocks.register();
        IlluminationParts.register();
        IlluminationMicroMaterials.register();
        IlluminationCreativeModeTabs.register();
    }

    public ProjectRedIllumination(ModContainer container, IEventBus modEventBus) {
        ProjectRedIllumination.container = container;

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        if (FMLEnvironment.dist.isClient()) {
            IlluminationClientInit.init(modEventBus);
        }

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        BLOCK_ENTITY_TYPES.register(modEventBus);
        PART_TYPES.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);

        modEventBus.register(new IlluminationMicroMaterials());
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();
        CompletableFuture<HolderLookup.Provider> provider = event.getLookupProvider();

        generator.addProvider(event.includeClient(), new IlluminationBlockStateModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new IlluminationItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new IlluminationLanguageProvider(output));

        generator.addProvider(event.includeServer(), new IlluminationLootTableProvider(output, provider));
        generator.addProvider(event.includeServer(), new IlluminationRecipeProvider(provider, output));
    }
}
