package mrtjp.projectred

import mrtjp.projectred.exploration._
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent, FMLDedicatedServerSetupEvent, FMLLoadCompleteEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

object ProjectRedExploration {
    final var MOD_ID = "projectred-exploration"
}

class ProjectRedExploration {

    ScorgeModLoadingContext.get.getModEventBus.register(this)
    ExplorationContent.register(ScorgeModLoadingContext.get.getModEventBus)

    @SubscribeEvent
    def onCommonSetup(event: FMLCommonSetupEvent) {
        ExplorationProxy.commonSetup(event)
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
