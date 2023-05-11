package mrtjp.projectred;

import codechicken.lib.gui.SimpleItemGroup;
import codechicken.lib.util.SneakyUtils;
import codechicken.multipart.api.MultiPartType;
import mrtjp.projectred.compatibility.ComputerCraftCompatibility;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.CoreNetwork;
import mrtjp.projectred.core.data.*;
import mrtjp.projectred.core.init.*;
import net.minecraft.block.Block;
import net.minecraft.block.Blocks;
import net.minecraft.data.DataGenerator;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.tileentity.TileEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.OptionalMod;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static mrtjp.projectred.ProjectRedCore.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedCore {

    public static final String MOD_ID = "projectred-core";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<TileEntityType<?>> TILE_ENTITIES = DeferredRegister.create(ForgeRegistries.TILE_ENTITIES, MOD_ID);
    public static final DeferredRegister<ContainerType<?>> CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<IRecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(ForgeRegistries.RECIPE_SERIALIZERS, MOD_ID);
    public static final DeferredRegister<MultiPartType<?>> PARTS = DeferredRegister.create(SneakyUtils.<Class<MultiPartType<?>>>unsafeCast(MultiPartType.class), MOD_ID);

    public static final SimpleItemGroup CORE_GROUP = new SimpleItemGroup(MOD_ID, () -> new ItemStack(CoreReferences.RED_ALLOY_INGOT_ITEM));

    static {
        CoreBlocks.register();
        CoreItems.register();
        CoreContainers.register();
        CoreParts.register();
    }

    public ProjectRedCore() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.safeRunWhenOn(Dist.CLIENT, () -> CoreClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        TILE_ENTITIES.register(modEventBus);
        CONTAINERS.register(modEventBus);
        PARTS.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        // Init packet handler
        CoreNetwork.init();

        // Load config file
        Configurator.load();

        // Load compatibility modules
        if (Configurator.compat_CCBundledCable) {
            OptionalMod.of("computercraft").ifPresent(ComputerCraftCompatibility::init);
        }
    }

    private void onGatherDataEvent(final GatherDataEvent event) {

        DataGenerator generator = event.getGenerator();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        if (event.includeClient()) {
            generator.addProvider(new CoreBlockStateModelProvider(generator, fileHelper));
            generator.addProvider(new CoreItemModelProvider(generator, fileHelper));
            generator.addProvider(new CoreLanguageProvider(generator));
        }
        if (event.includeServer()) {
            generator.addProvider(new CoreRecipeProvider(generator));
            generator.addProvider(new CoreLootTableProvider(generator));
            generator.addProvider(new CoreItemTagsProvider(generator, fileHelper));
        }
    }
}
