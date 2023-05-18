package mrtjp.projectred.expansion;

import codechicken.lib.gui.SimpleItemGroup;
import codechicken.lib.util.SneakyUtils;
import codechicken.multipart.api.MultiPartType;
import mrtjp.projectred.expansion.data.*;
import mrtjp.projectred.expansion.init.*;
import net.minecraft.block.Block;
import net.minecraft.data.DataGenerator;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedExpansion {

    public static final String MOD_ID = "projectred-expansion";

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<TileEntityType<?>> TILE_ENTITIES = DeferredRegister.create(ForgeRegistries.TILE_ENTITIES, MOD_ID);
    public static final DeferredRegister<ContainerType<?>> CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, MOD_ID);
    public static final DeferredRegister<MultiPartType<?>> PARTS = DeferredRegister.create(SneakyUtils.<Class<MultiPartType<?>>>unsafeCast(MultiPartType.class), MOD_ID);

    public static final SimpleItemGroup EXPANSION_GROUP = new SimpleItemGroup(MOD_ID, () -> new ItemStack(ExpansionReferences.PROJECT_BENCH_BLOCK));

    static {
        ExpansionBlocks.register();
        ExpansionContainers.register();
        ExpansionItems.register();
//        ExpansionParts.register();
    }

    public ProjectRedExpansion() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.safeRunWhenOn(Dist.CLIENT, () -> ExpansionClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        TILE_ENTITIES.register(modEventBus);
        CONTAINERS.register(modEventBus);
        PARTS.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

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
            generator.addProvider(new ExpansionRecipeProvider(generator));
            generator.addProvider(new ExpansionLootTableProvider(generator));
        }
    }
}
