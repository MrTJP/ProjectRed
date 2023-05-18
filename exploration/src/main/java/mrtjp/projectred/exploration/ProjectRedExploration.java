package mrtjp.projectred.exploration;

import codechicken.lib.gui.SimpleItemGroup;
import mrtjp.projectred.exploration.data.*;
import mrtjp.projectred.exploration.init.*;
import net.minecraft.block.Block;
import net.minecraft.data.DataGenerator;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.world.gen.carver.WorldCarver;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationReferences.MARBLE_BRICK_BLOCK;

@Mod(MOD_ID)
public class ProjectRedExploration {

    public static final String MOD_ID = "projectred-exploration";

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<ContainerType<?>> CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, MOD_ID);
    public static final DeferredRegister<WorldCarver<?>> WORLD_CARVERS = DeferredRegister.create(ForgeRegistries.WORLD_CARVERS, MOD_ID);
    public static final DeferredRegister<IRecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(ForgeRegistries.RECIPE_SERIALIZERS, MOD_ID);

    public static final SimpleItemGroup EXPLORATION_GROUP = new SimpleItemGroup(MOD_ID, () -> new ItemStack(MARBLE_BRICK_BLOCK));

    static {
        ExplorationBlocks.register();
        ExplorationItems.register();
        ExplorationContainers.register();
        ExplorationWorldFeatures.register();
        ExplorationRecipeSerializers.register();
    }

    public ProjectRedExploration() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.safeRunWhenOn(Dist.CLIENT, () -> ExplorationClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        CONTAINERS.register(modEventBus);
        WORLD_CARVERS.register(modEventBus);
        RECIPE_SERIALIZERS.register(modEventBus);

        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGH, ExplorationWorldFeatures::onBiomeLoadingEvent);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

        event.enqueueWork(ExplorationWorldFeatures::load);
    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        if (event.includeClient()) {
            generator.addProvider(new ExplorationBlockStateModelProvider(generator, fileHelper));
            generator.addProvider(new ExplorationItemModelProvider(generator, fileHelper));
            generator.addProvider(new ExplorationLanguageProvider(generator));
        }
        if (event.includeServer()) {
            generator.addProvider(new ExplorationBlockTagsProvider(generator, fileHelper));
            generator.addProvider(new ExplorationItemTagsProvider(generator, fileHelper));
            generator.addProvider(new ExplorationLootTableProvider(generator));
            generator.addProvider(new ExplorationRecipeProvider(generator));
        }
    }
}
