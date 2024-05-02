package mrtjp.projectred.integration;

import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.integration.data.IntegrationItemModelProvider;
import mrtjp.projectred.integration.data.IntegrationLanguageProvider;
import mrtjp.projectred.integration.data.IntegrationRecipeProvider;
import mrtjp.projectred.integration.init.IntegrationClientInit;
import mrtjp.projectred.integration.init.IntegrationCreativeModeTabs;
import mrtjp.projectred.integration.init.IntegrationParts;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
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

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedIntegration {

    public static final String MOD_ID = "projectred_integration";

    public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<MultipartType<?>> PART_TYPES = DeferredRegister.create(MultipartType.MULTIPART_TYPES, MOD_ID);
    public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, MOD_ID);

    static {
        IntegrationParts.register();
        IntegrationCreativeModeTabs.register();
    }

    public ProjectRedIntegration() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> IntegrationClientInit::init);

        ITEMS.register(modEventBus);
        PART_TYPES.register(modEventBus);
        CREATIVE_TABS.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        IntegrationNetwork.init();
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
