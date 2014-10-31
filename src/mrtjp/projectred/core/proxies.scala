package mrtjp.projectred.core

import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.common.FMLCommonHandler
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedCore._
import mrtjp.projectred.core.libmc.fx.{ParticleIconRegistry, ParticleManagement}
import mrtjp.projectred.core.libmc.recipe.RecipeLib
import net.minecraftforge.common.MinecraftForge

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

        RecipeLib.loadLib()
        CoreRecipes.initCoreRecipes()
    }

    def postinit(){}

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class CoreProxy_client extends CoreProxy_server
{
    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()
        MinecraftForge.EVENT_BUS.register(ParticleManagement.instance)
        FMLCommonHandler.instance().bus().register(ParticleManagement.instance)
        MinecraftForge.EVENT_BUS.register(ParticleIconRegistry.instance)
        MinecraftForge.EVENT_BUS.register(RenderHalo)

        new PRUpdateChecker
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        PacketCustom.assignHandler(CoreCPH.channel, CoreCPH)
    }
}

object CoreProxy extends CoreProxy_client