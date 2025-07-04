package mrtjp.projectred.fabrication;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.fabrication.data.*;
import mrtjp.projectred.fabrication.init.*;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
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
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedFabrication {

    public static final String MOD_ID = "projectred_fabrication";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(BuiltInRegistries.BLOCK, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, MOD_ID);
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE, MOD_ID);
    public static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(BuiltInRegistries.MENU, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PARTS = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);
    public static final DeferredRegister<DataComponentType<?>> DATA_COMPONENT_TYPES = DeferredRegister.create(BuiltInRegistries.DATA_COMPONENT_TYPE, MOD_ID);

    private static @Nullable ModContainer container;

    static {
        FabricationBlocks.register();
        FabricationMenus.register();
        FabricationItems.register();
        FabricationParts.register();
        FabricationCreativeModeTab.register();
        FabricationDataComponents.register();
    }

    public ProjectRedFabrication(ModContainer container, IEventBus modEventBus) {
        ProjectRedFabrication.container = container;

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        if (FMLEnvironment.dist.isClient()) {
            FabricationClientInit.init(modEventBus);
        }

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        BLOCK_ENTITY_TYPES.register(modEventBus);
        MENU_TYPES.register(modEventBus);
        PARTS.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);
        DATA_COMPONENT_TYPES.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        generator.addProvider(event.includeClient(), new FabricationBlockStateModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new FabricationItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new FabricationLanguageProvider(output));

        generator.addProvider(event.includeServer(), new FabricationBlockTagsProvider(output, event.getLookupProvider(), fileHelper));
        generator.addProvider(event.includeServer(), new FabricationLootTableProvider(output, event.getLookupProvider()));
        generator.addProvider(event.includeServer(), new FabricationRecipeProvider(event.getLookupProvider(), output));
    }
}
