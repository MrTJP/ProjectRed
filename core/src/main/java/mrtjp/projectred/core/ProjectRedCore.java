package mrtjp.projectred.core;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.compatibility.ComputerCraftCompatibility;
import mrtjp.projectred.core.data.*;
import mrtjp.projectred.core.init.*;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.data.BlockTagsProvider;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.data.event.GatherDataEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.OptionalMod;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
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
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(ForgeRegistries.BLOCK_ENTITY_TYPES, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(ForgeRegistries.MENU_TYPES, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<RecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(ForgeRegistries.RECIPE_SERIALIZERS, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    static {
        CoreBlocks.register();
        CoreItems.register();
        CoreMenus.register();
        CoreParts.register();
        CoreCreativeModeTabs.register();
    }

    public ProjectRedCore() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> CoreClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        BLOCK_ENTITY_TYPES.register(modEventBus);
        MENU_TYPES.register(modEventBus);
        PART_TYPES.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        // Init packet handler
        CoreNetwork.init();

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
}
