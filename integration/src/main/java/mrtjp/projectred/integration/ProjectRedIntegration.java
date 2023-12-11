package mrtjp.projectred.integration;

import codechicken.lib.gui.SimpleCreativeTab;
import codechicken.multipart.api.MultipartType;
import mrtjp.projectred.integration.data.IntegrationItemModelProvider;
import mrtjp.projectred.integration.data.IntegrationLanguageProvider;
import mrtjp.projectred.integration.data.IntegrationRecipeProvider;
import mrtjp.projectred.integration.init.IntegrationClientInit;
import mrtjp.projectred.integration.init.IntegrationParts;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.item.Item;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.forge.event.lifecycle.GatherDataEvent;
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

    public static final SimpleCreativeTab CREATIVE_TAB = new SimpleCreativeTab(MOD_ID, GateType.OR::makeStack);

    static {
        IntegrationParts.register();
    }

    public ProjectRedIntegration() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> IntegrationClientInit::init);

        ITEMS.register(modEventBus);
        PART_TYPES.register(modEventBus);
    }

    private void commonSetup(final FMLCommonSetupEvent event) {
        IntegrationNetwork.init();
    }

    private void onGatherDataEvent(final GatherDataEvent event) {
        DataGenerator generator = event.getGenerator();
        ExistingFileHelper fileHelper = event.getExistingFileHelper();

        if (event.includeClient()) {
            generator.addProvider(new IntegrationItemModelProvider(generator, fileHelper));
            generator.addProvider(new IntegrationLanguageProvider(generator));
        }
        if (event.includeServer()) {
            generator.addProvider(new IntegrationRecipeProvider(generator));
        }
    }
}
