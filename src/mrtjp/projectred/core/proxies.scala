package mrtjp.projectred.core

import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedCore._
import net.minecraftforge.common.MinecraftForge
import mrtjp.projectred.core.libmc.fx.{ParticleIconRegistry, ParticleManagement}
import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.client.registry.RenderingRegistry
import cpw.mods.fml.common.FMLCommonHandler
import mrtjp.projectred.core.libmc.{MultiRenderHandler, RenderLib}

class CoreProxy_server extends IProxy
{
    def preinit()
    {
        MinecraftForge.EVENT_BUS.register(RetroactiveWorldGenerator.instance)
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
    override def postinit()
    {
        super.postinit()
        MinecraftForge.EVENT_BUS.register(Messenger)
        MinecraftForge.EVENT_BUS.register(ParticleManagement.instance)
        FMLCommonHandler.instance().bus().register(ParticleManagement.instance)
        MinecraftForge.EVENT_BUS.register(ParticleIconRegistry.instance)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        PacketCustom.assignHandler(CoreCPH.channel, CoreCPH)
        GuiManager.initBuilders()

        RenderLib.multiRenderID = RenderingRegistry.getNextAvailableRenderId
        RenderingRegistry.registerBlockHandler(MultiRenderHandler)
    }
}

object CoreProxy extends CoreProxy_client
