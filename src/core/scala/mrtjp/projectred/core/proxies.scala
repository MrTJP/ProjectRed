package mrtjp.projectred.core

import mrtjp.projectred.core.gui.screen.inventory.ElectrotineGeneratorScreen
import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorContainer
import net.minecraft.client.gui.ScreenManager
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent}

class CoreProxy_server extends IProxy {
    override def commonSetup(event: FMLCommonSetupEvent) {
        CoreNetwork.init()
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

        // Register screens
        ScreenManager.register(
            CoreContent.electrotineGeneratorContainer.get,
            (cont: ElectrotineGeneratorContainer, inv, text) => new ElectrotineGeneratorScreen(cont, inv, text)
        )
    }
}

object CoreProxy extends CoreProxy_client
