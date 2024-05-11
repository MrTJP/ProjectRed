package mrtjp.projectred.illumination;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.gui.SimpleCreativeTab;
import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.illumination.data.*;
import mrtjp.projectred.illumination.init.IlluminationBlocks;
import mrtjp.projectred.illumination.init.IlluminationClientInit;
import mrtjp.projectred.illumination.init.IlluminationMicroMaterials;
import mrtjp.projectred.illumination.init.IlluminationParts;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
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

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedIllumination {

    public static final String MOD_ID = "projectred_illumination";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(ForgeRegistries.BLOCK_ENTITY_TYPES, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);

    public static final SimpleCreativeTab ILLUMINATION_GROUP = new SimpleCreativeTab(MOD_ID, () -> new ItemStack(BlockLightType.ILLUMAR_LAMP.getBlock(EnumColour.RED.ordinal(), true)));

    static {
        IlluminationBlocks.register();
        IlluminationParts.register();
        IlluminationMicroMaterials.register();
    }

    public ProjectRedIllumination() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> IlluminationClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        BLOCK_ENTITY_TYPES.register(modEventBus);
        PART_TYPES.register(modEventBus);

        modEventBus.register(new IlluminationMicroMaterials());
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        generator.addProvider(event.includeClient(), new IlluminationBlockStateModelProvider(generator, fileHelper));
        generator.addProvider(event.includeClient(), new IlluminationItemModelProvider(generator, fileHelper));
        generator.addProvider(event.includeClient(), new IlluminationLanguageProvider(generator));

        generator.addProvider(event.includeServer(), new IlluminationBlockLootProvider(generator));
        generator.addProvider(event.includeServer(), new IlluminationRecipeProvider(generator));
    }
}
