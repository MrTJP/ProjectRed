package mrtjp.projectred.core

import cpw.mods.fml.common.registry.TickRegistry
import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedCore._
import mrtjp.projectred.core.ItemPart.EnumPart
import net.minecraftforge.common.MinecraftForge
import mrtjp.projectred.core.fx.{ParticleIconRegistry, ParticleManagement}
import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.client.registry.RenderingRegistry

class CoreProxy_server extends IProxy
{
    def preinit()
    {
        MinecraftForge.EVENT_BUS.register(RetroactiveWorldGenerator.instance)
        TickRegistry.registerTickHandler(RetroactiveWorldGenerator.instance, Side.SERVER)
    }

    def init()
    {
        PacketCustom.assignHandler(CoreSPH.channel, CoreSPH)

        itemComponent = new ItemPart(Configurator.item_componentsID.getInt)
        itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt)
        itemScrewdriver = new ItemScrewdriver(Configurator.item_screwdriverID.getInt)
        itemWireDebugger = new ItemWireDebugger(Configurator.item_wireDebuggerID.getInt)
        itemDataCard = new ItemDataCard(Configurator.item_dataCardID.getInt)
        EnumPart.initOreDictDefinitions()
    }

    def postinit()
    {
        CoreRecipes.initCoreRecipes()
    }
}

class CoreProxy_client extends CoreProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        MinecraftForge.EVENT_BUS.register(ParticleManagement.instance)
        MinecraftForge.EVENT_BUS.register(ParticleIconRegistry.instance)
        TickRegistry.registerTickHandler(ParticleManagement.instance, Side.CLIENT)
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
