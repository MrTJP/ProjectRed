package mrtjp.projectred.illumination;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.gui.SimpleItemGroup;
import codechicken.lib.util.SneakyUtils;
import codechicken.multipart.api.MultiPartType;
import mrtjp.projectred.illumination.data.*;
import mrtjp.projectred.illumination.init.IlluminationBlocks;
import mrtjp.projectred.illumination.init.IlluminationClientInit;
import mrtjp.projectred.illumination.init.IlluminationMicroMaterials;
import mrtjp.projectred.illumination.init.IlluminationParts;
import net.minecraft.block.Block;
import net.minecraft.data.DataGenerator;
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

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedIllumination {

    public static final String MOD_ID = "projectred-illumination";

    public static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID);
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<TileEntityType<?>> TILE_ENTITIES = DeferredRegister.create(ForgeRegistries.TILE_ENTITIES, MOD_ID);
    public static final DeferredRegister<MultiPartType<?>> PARTS = DeferredRegister.create(SneakyUtils.<Class<MultiPartType<?>>>unsafeCast(MultiPartType.class), MOD_ID);

    public static final SimpleItemGroup ILLUMINATION_GROUP = new SimpleItemGroup(MOD_ID, () -> new ItemStack(BlockLightType.ILLUMAR_LAMP.getBlock(EnumColour.RED.ordinal(), true)));

    static {
        IlluminationBlocks.register();
        IlluminationParts.register();
        IlluminationMicroMaterials.register();
    }

    public ProjectRedIllumination() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.safeRunWhenOn(Dist.CLIENT, () -> IlluminationClientInit::init);

        BLOCKS.register(modEventBus);
        ITEMS.register(modEventBus);
        TILE_ENTITIES.register(modEventBus);
        PARTS.register(modEventBus);

        modEventBus.register(new IlluminationMicroMaterials());
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        if (event.includeClient()) {
            generator.addProvider(new IlluminationBlockStateModelProvider(generator, fileHelper));
            generator.addProvider(new IlluminationItemModelProvider(generator, fileHelper));
            generator.addProvider(new IlluminationLanguageProvider(generator));
        }
        if (event.includeServer()) {
            generator.addProvider(new IlluminationBlockLootProvider(generator));
            generator.addProvider(new IlluminationRecipeProvider(generator));
        }
    }
}
