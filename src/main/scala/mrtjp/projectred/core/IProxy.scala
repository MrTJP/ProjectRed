package mrtjp.projectred.core

import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent, FMLDedicatedServerSetupEvent, FMLLoadCompleteEvent}

trait IProxy {

    def construct() {
    }

    def commonSetup(event: FMLCommonSetupEvent) {}

    def clientSetup(event: FMLClientSetupEvent) {}

    def serverSetup(event: FMLDedicatedServerSetupEvent) {}

    def loadComplete(event: FMLLoadCompleteEvent) {}
}
