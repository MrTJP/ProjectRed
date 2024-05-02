package mrtjp.projectred.transmission;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.transmission.data.*;
import mrtjp.projectred.transmission.init.TransmissionClientInit;
import mrtjp.projectred.transmission.init.TransmissionCreativeModeTabs;
import mrtjp.projectred.transmission.init.TransmissionParts;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.common.data.BlockTagsProvider;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.data.event.GatherDataEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedTransmission {

    public static final String MOD_ID = "projectred_transmission";

    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    static {
        ProjectRedAPI.transmissionAPI = TransmissionAPI.INSTANCE;

        TransmissionParts.register();
        TransmissionCreativeModeTabs.register();
    }

    public ProjectRedTransmission() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> TransmissionClientInit::init);

        ITEMS.register(modEventBus);
        PART_TYPES.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);

        MinecraftForge.EVENT_BUS.addListener(this::onServerStartEvent);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        generator.addProvider(event.includeClient(), new TransmissionItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new TransmissionLanguageProvider(output));

        BlockTagsProvider blockTagsProvider = new TransmissionBlockTagsProvider(output, event.getLookupProvider(), fileHelper);
        generator.addProvider(event.includeServer(), blockTagsProvider);
        generator.addProvider(event.includeServer(), new TransmissionItemTagsProvider(output, event.getLookupProvider(), blockTagsProvider.contentsGetter(), fileHelper));
        generator.addProvider(event.includeServer(), new TransmissionRecipeProvider(output));
    }

    private void onServerStartEvent(final ServerAboutToStartEvent event) {
        RedstonePropagator.resetPowerFlags();
    }
}
