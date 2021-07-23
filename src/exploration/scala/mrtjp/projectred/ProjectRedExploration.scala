package mrtjp.projectred

import mrtjp.projectred.exploration._
import mrtjp.projectred.exploration.init.ExplorationFeatures
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent, FMLDedicatedServerSetupEvent, FMLLoadCompleteEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

import scala.jdk.FunctionWrappers.AsJavaSupplier

object ProjectRedExploration {
    final var MOD_ID = "projectred-exploration"
}

class ProjectRedExploration {

    {
        val modEventBus = ScorgeModLoadingContext.get.getModEventBus
        modEventBus.register(this)

        ExplorationContent.register(modEventBus)
        ExplorationFeatures.register(modEventBus)
    }

    @SubscribeEvent
    def onCommonSetup(event: FMLCommonSetupEvent) {
        ExplorationProxy.commonSetup(event)
        event.enqueueWork(AsJavaSupplier(() => ExplorationFeatures.load()))
    }

    @SubscribeEvent
    def onClientSetup(event: FMLClientSetupEvent) {
        ExplorationProxy.clientSetup(event)
    }

    @SubscribeEvent
    def onServerSetup(event: FMLDedicatedServerSetupEvent) {
        ExplorationProxy.serverSetup(event)
    }

    @SubscribeEvent
    def onLoadComplete(event: FMLLoadCompleteEvent) {
        ExplorationProxy.loadComplete(event)
    }
}
