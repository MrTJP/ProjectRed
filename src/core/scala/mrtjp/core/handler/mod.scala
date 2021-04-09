/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.handler

import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent, FMLDedicatedServerSetupEvent, FMLLoadCompleteEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

object MrTJPCoreMod {
    final val MOD_ID = "mrtjpcore"
}

class MrTJPCoreMod {

    MrTJPCoreNetwork.init()
    ScorgeModLoadingContext.get.getModEventBus.register(this)

    @SubscribeEvent
    def onCommonSetup(event: FMLCommonSetupEvent) {
        MrTJPCoreProxy.commonSetup(event)
    }

    @SubscribeEvent
    def onClientSetup(event: FMLClientSetupEvent) {
        MrTJPCoreProxy.clientSetup(event)
    }

    @SubscribeEvent
    def onServerSetup(event: FMLDedicatedServerSetupEvent) {
        MrTJPCoreProxy.serverSetup(event)
    }

    @SubscribeEvent
    def onLoadComplete(event: FMLLoadCompleteEvent) {
        MrTJPCoreProxy.loadComplete(event)
    }
}

//object MrTJPConfig extends ModConfig("mrtjpcore")
//{
//    var retro_gen = false
//    var retro_gen_id = "mrtjp_gen"
//
//    var check_versions = true
//    var check_unstable = false
//
//    override def getFileName = "MrTJPCore"
//
//    override protected def initValues()
//    {
//        val general = new BaseCategory("General", "General settings for MrTJPCore")
//        check_versions = general.put("Version Checking", check_versions, "Flag to enable or disable the update checker.")
//        check_unstable = general.put("Include Unstable", check_unstable, "Flag to set if the update checker should consider unstable builds as a new version.")
//
//        val gen = new BaseCategory("World Gen", "Settings related to world generation")
//        retro_gen = gen.put("Retroactive World Generation", retro_gen, "Toggle to enable retrogeneration, a feature that would allow ores to be generated after the world has been created.")
//        retro_gen_id = gen.put("RetroGen ID", retro_gen_id, "The database ID that is used to store which chunks have been generated already. Changing this will cause generation to run again on the same chunk.")
//    }
//}

//class MrTJPConfigGui(parent:GuiScreen) extends SpecialConfigGui(parent, "mrtjpcore", MrTJPConfig.config)
//class GuiConfigFactory extends TModGuiFactory
//{
//    override def createConfigGui(parentScreen:GuiScreen) = new MrTJPConfigGui(parentScreen)
//}
