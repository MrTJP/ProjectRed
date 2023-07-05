package mrtjp.projectred.core;

import codechicken.lib.gui.SimpleCreativeTab;
import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.compatibility.ComputerCraftCompatibility;
import mrtjp.projectred.core.data.*;
import mrtjp.projectred.core.init.*;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.OptionalMod;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.forge.event.lifecycle.GatherDataEvent;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedCore {

    public static final String MOD_ID = "projectred_core";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITIES = DeferredRegister.create(ForgeRegistries.BLOCK_ENTITIES, MOD_ID);
    public static final DeferredRegister<MenuType<?>> CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<RecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(ForgeRegistries.RECIPE_SERIALIZERS, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PARTS = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);

    public static final SimpleCreativeTab CORE_CREATIVE_TAB = new SimpleCreativeTab(MOD_ID, () -> new ItemStack(CoreReferences.RED_ALLOY_INGOT_ITEM));

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

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> CoreClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        BLOCK_ENTITIES.register(modEventBus);
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
            OptionalMod.of("computercraft").ifPresent(mod -> ComputerCraftCompatibility.init(mod));
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
            generator.addProvider(new CoreBlockTagsProvider(generator, fileHelper));
        }
    }
}
