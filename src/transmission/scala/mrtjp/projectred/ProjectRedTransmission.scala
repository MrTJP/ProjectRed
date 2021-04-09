package mrtjp.projectred

import mrtjp.projectred.api.ProjectRedAPI
import mrtjp.projectred.transmission._
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.DistExecutor
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent, FMLDedicatedServerSetupEvent, FMLLoadCompleteEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext
import mrtjp.projectred.ProjectRedTransmission._
import mrtjp.projectred.core.WirePropagator
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent

object ProjectRedTransmission {
    final var MOD_ID = "projectred-transmission"
    final var proxy:TransmissionProxy = DistExecutor.safeRunForDist(
        () => () => new TransmissionProxyClient().asInstanceOf[TransmissionProxy],
        () => () => new TransmissionProxy())
}
class ProjectRedTransmission
{
    ProjectRedAPI.transmissionAPI = new APIImpl_Transmission
    proxy.construct()
    ScorgeModLoadingContext.get.getModEventBus.register(this)
    TransmissionContent.register(ScorgeModLoadingContext.get.getModEventBus)
    MinecraftForge.EVENT_BUS.addListener(serverStarting)

    @SubscribeEvent
    def onCommonSetup(event: FMLCommonSetupEvent) {
        proxy.commonSetup(event)
    }

    @SubscribeEvent
    def onClientSetup(event: FMLClientSetupEvent) {
        proxy.clientSetup(event)
    }

    @SubscribeEvent
    def onServerSetup(event: FMLDedicatedServerSetupEvent) {
        proxy.serverSetup(event)
    }

    @SubscribeEvent
    def onLoadComplete(event: FMLLoadCompleteEvent) {
        proxy.loadComplete(event)
    }

    def serverStarting(event:FMLServerAboutToStartEvent)
    {
        WirePropagator.reset()
    }
}
