package mrtjp.projectred.core

import codechicken.lib.packet.PacketCustom
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent}

class CoreProxy_server extends IProxy {
    override def commonSetup(event: FMLCommonSetupEvent) {
    }

    def init() {
//        PacketCustom.assignHandler(CoreSPH.channel, CoreSPH)
    }

    def postinit() {}
}

class CoreProxy_client extends CoreProxy_server {
//    @OnlyIn(Dist.CLIENT)
//    override def preinit() {
//        super.preinit()
//        //RecipeDumper.load()
//    }


//    @OnlyIn(Dist.CLIENT)
//    override def init() {
//        super.init()
//        PacketCustom.assignHandler(CoreCPH.channel, CoreCPH)
//    }
//
//    @OnlyIn(Dist.CLIENT)
//    override def postinit() {
//        super.postinit()
//        MinecraftForge.EVENT_BUS.register(RenderHalo)
//    }

    @OnlyIn(Dist.CLIENT)
    override def clientSetup(event:FMLClientSetupEvent):Unit = {
        super.clientSetup(event)
        MinecraftForge.EVENT_BUS.register(RenderHalo)
    }
}

object CoreProxy extends CoreProxy_client
