package mrtjp.projectred.transmission;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.core.RedstonePropagator;
import mrtjp.projectred.transmission.data.*;
import mrtjp.projectred.transmission.init.TransmissionClientInit;
import mrtjp.projectred.transmission.init.TransmissionCreativeModeTabs;
import mrtjp.projectred.transmission.init.TransmissionParts;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.event.server.ServerAboutToStartEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedTransmission {

    public static final String MOD_ID = "projectred_transmission";

    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    private static @Nullable ModContainer container;

    static {
        ProjectRedAPI.transmissionAPI = TransmissionAPI.INSTANCE;

        TransmissionParts.register();
        TransmissionCreativeModeTabs.register();
    }

    public ProjectRedTransmission(ModContainer container, IEventBus modEventBus) {
        ProjectRedTransmission.container = container;

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        if (FMLEnvironment.dist.isClient()) {
            TransmissionClientInit.init(modEventBus);
        }

        ITEMS.register(modEventBus);
        PART_TYPES.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);

        NeoForge.EVENT_BUS.addListener(this::onServerStartEvent);
    }

    public static ModContainer getContainer() {
        return Objects.requireNonNull(container);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {

    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        PackOutput output = generator.getPackOutput();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();
        CompletableFuture<HolderLookup.Provider> provider = event.getLookupProvider();

        generator.addProvider(event.includeClient(), new TransmissionItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new TransmissionLanguageProvider(output));

        BlockTagsProvider blockTagsProvider = new TransmissionBlockTagsProvider(output, provider, fileHelper);
        generator.addProvider(event.includeServer(), blockTagsProvider);
        generator.addProvider(event.includeServer(), new TransmissionItemTagsProvider(output, provider, blockTagsProvider.contentsGetter(), fileHelper));
        generator.addProvider(event.includeServer(), new TransmissionRecipeProvider(provider, output));
    }

    private void onServerStartEvent(final ServerAboutToStartEvent event) {
        RedstonePropagator.resetPowerFlags();
    }
}
