package mrtjp.projectred.core

import codechicken.lib.packet.PacketCustom
import mrtjp.projectred.ProjectRedCore._
import mrtjp.projectred.core.libmc.recipe.RecipeLib
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class CoreProxy_server extends IProxy
{
    def preinit()
    {
        itemPart = new ItemPart
        itemDrawPlate = new ItemDrawPlate
        itemScrewdriver = new ItemScrewdriver
        itemWireDebugger = new ItemWireDebugger
    }

    def init()
    {
        PacketCustom.assignHandler(CoreSPH.channel, CoreSPH)

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
    override def preinit()
    {
        super.preinit()

        for (i <- PartDefs.values) i.setCustomModelResourceLocations()
        ModelLoader.setCustomModelResourceLocation(itemDrawPlate, 0, new ModelResourceLocation("projectred:base/itemTools", "type=draw_plate"))
        ModelLoader.setCustomModelResourceLocation(itemScrewdriver, 0, new ModelResourceLocation("projectred:base/itemTools", "type=screwdriver"))
        ModelLoader.setCustomModelResourceLocation(itemWireDebugger, 0, new ModelResourceLocation("projectred:base/itemTools", "type=multimeter"))
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        PacketCustom.assignHandler(CoreCPH.channel, CoreCPH)
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()
        MinecraftForge.EVENT_BUS.register(RenderHalo)

        new PRUpdateChecker
    }
}

object CoreProxy extends CoreProxy_client