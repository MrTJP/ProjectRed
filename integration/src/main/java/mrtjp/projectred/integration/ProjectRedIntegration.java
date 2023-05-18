package mrtjp.projectred.integration;

import codechicken.lib.gui.SimpleItemGroup;
import codechicken.lib.util.SneakyUtils;
import codechicken.multipart.api.MultiPartType;
import mrtjp.projectred.integration.data.IntegrationItemModelProvider;
import mrtjp.projectred.integration.data.IntegrationLanguageProvider;
import mrtjp.projectred.integration.data.IntegrationRecipeProvider;
import mrtjp.projectred.integration.init.IntegrationClientInit;
import mrtjp.projectred.integration.init.IntegrationParts;
import net.minecraft.data.DataGenerator;
import net.minecraft.item.Item;
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

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

@Mod(MOD_ID)
public class ProjectRedIntegration {

    public static final String MOD_ID = "projectred-integration";

    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID);
    public static final DeferredRegister<MultiPartType<?>> PARTS = DeferredRegister.create(SneakyUtils.<Class<MultiPartType<?>>>unsafeCast(MultiPartType.class), MOD_ID);

    public static final SimpleItemGroup INTEGRATION_GROUP = new SimpleItemGroup(MOD_ID, GateType.OR::makeStack);

    static {
        IntegrationParts.register();
    }

    public ProjectRedIntegration() {
        final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::onGatherDataEvent);

        DistExecutor.safeRunWhenOn(Dist.CLIENT, () -> IntegrationClientInit::init);

        ITEMS.register(modEventBus);
        PARTS.register(modEventBus);
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
