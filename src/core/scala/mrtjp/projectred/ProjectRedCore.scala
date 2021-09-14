package mrtjp.projectred

import mrtjp.projectred.compatibility.ComputerCraftCompatibility
import mrtjp.projectred.core._
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.OptionalMod
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent, FMLDedicatedServerSetupEvent, FMLLoadCompleteEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

object ProjectRedCore {
    final val MOD_ID = "projectred-core"
}

class ProjectRedCore {

    Configurator.loadConfig()
    ScorgeModLoadingContext.get.getModEventBus.register(this)
    CoreContent.register(ScorgeModLoadingContext.get.getModEventBus)

    @SubscribeEvent
    def onCommonSetup(event: FMLCommonSetupEvent) {
        CoreProxy.commonSetup(event)

        // Compatibility modules
        OptionalMod.of[Any]("computercraft").ifPresent(_ => ComputerCraftCompatibility.init())
    }

    @SubscribeEvent
    def onClientSetup(event: FMLClientSetupEvent) {
        CoreProxy.clientSetup(event)
    }

    @SubscribeEvent
    def onServerSetup(event: FMLDedicatedServerSetupEvent) {
        CoreProxy.serverSetup(event)
    }

    @SubscribeEvent
    def onLoadComplete(event: FMLLoadCompleteEvent) {
        CoreProxy.loadComplete(event)
    }
}
