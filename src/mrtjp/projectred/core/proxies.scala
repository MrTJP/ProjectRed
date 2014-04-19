package mrtjp.projectred.core

import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedCore._
import net.minecraftforge.common.MinecraftForge
import mrtjp.projectred.core.libmc.fx.{ParticleIconRegistry, ParticleManagement}
import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.client.registry.RenderingRegistry
import mrtjp.projectred.core.libmc.BasicRenderUtils

class CoreProxy_server extends IProxy
{
    def preinit()
    {
        MinecraftForge.EVENT_BUS.register(RetroactiveWorldGenerator.instance)
        //TickRegistry.registerTickHandler(RetroactiveWorldGenerator.instance, Side.SERVER)
    }

    def init()
    {
        PacketCustom.assignHandler(CoreSPH.channel, CoreSPH)

        itemPart = new ItemPart
        itemDrawPlate = new ItemDrawPlate
        itemScrewdriver = new ItemScrewdriver
        itemWireDebugger = new ItemWireDebugger
        itemDataCard = new ItemDataCard
    }

    def postinit()
    {
        CoreRecipes.initCoreRecipes()
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class CoreProxy_client extends CoreProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        MinecraftForge.EVENT_BUS.register(ParticleManagement.instance)
        MinecraftForge.EVENT_BUS.register(ParticleIconRegistry.instance)
        //TickRegistry.registerTickHandler(ParticleManagement.instance, Side.CLIENT)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MinecraftForge.EVENT_BUS.register(Messenger)
        PacketCustom.assignHandler(CoreCPH.channel, CoreCPH)
        BasicRenderUtils.coreRenderHandlerID = RenderingRegistry.getNextAvailableRenderId
        RenderingRegistry.registerBlockHandler(BasicRenderUtils.MultiRenderHandler.instance)
    }
}

object CoreProxy extends CoreProxy_client
