package mrtjp.projectred.integration;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.integration.data.IntegrationItemModelProvider;
import mrtjp.projectred.integration.data.IntegrationLanguageProvider;
import mrtjp.projectred.integration.data.IntegrationRecipeProvider;
import mrtjp.projectred.integration.init.IntegrationClientInit;
import mrtjp.projectred.integration.init.IntegrationCreativeModeTabs;
import mrtjp.projectred.integration.init.IntegrationParts;
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
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedIntegration {

    public static final String MOD_ID = "projectred_integration";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    private static @Nullable ModContainer container;

    static {
        IntegrationParts.register();
        IntegrationCreativeModeTabs.register();
    }

    public ProjectRedIntegration(ModContainer container, IEventBus modEventBus) {
        ProjectRedIntegration.container = container;

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        if (FMLEnvironment.dist.isClient()) {
            IntegrationClientInit.init(modEventBus);
        }

        IntegrationNetwork.init(modEventBus);

        ITEMS.register(modEventBus);
        PART_TYPES.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);
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

        generator.addProvider(event.includeClient(), new IntegrationItemModelProvider(output, fileHelper));
        generator.addProvider(event.includeClient(), new IntegrationLanguageProvider(output));

        generator.addProvider(event.includeServer(), new IntegrationRecipeProvider(output));
    }
}
