package mrtjp.projectred.illumination

import codechicken.lib.model.ModelRegistryHelper
import mrtjp.core.block.MultiTileRenderRegistry
import mrtjp.projectred.ProjectRedIllumination._
import mrtjp.projectred.core.IProxy
import net.minecraftforge.fml.client.registry.ClientRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class IlluminationProxy_server extends IProxy
{
    val lights = Seq(LightFactoryLantern, LightFactoryFixture, LightFactoryFallout, LightFactoryCage)

    override def preinit()
    {
        blockLamp = new BlockLamp
        blockLamp.addTile(classOf[TileLamp], 0)
        itemBlockLamp = new ItemBlockLamp

        blockAirousLight = new BlockAirousLight
        blockAirousLight.bindTile(classOf[TileAirousLight])

        lights.foreach(_.register())
    }

    override def init()
    {
//        MultiPartRegistry.registerParts(this, (lights.map(_.getType) :+ "pr_lightbutton" :+ "pr_flightbutton").toArray)
//        for (l <- lights) l.initServer()

//        itemPartIllumarButton = new ItemPartButton
//        itemPartIllumarFButton = new ItemPartFButton


//        IlluminationRecipes.initRecipes()
//
        LightMicroMaterial.register()
    }

    override def postinit(){}

//    private def getLight(name:String) = lights.find(_.getType == name) match
//    {
//        case Some(e) => e.createPart
//        case None => null
//    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class IlluminationProxy_client extends IlluminationProxy_server
{

    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()

        ModelRegistryHelper.registerItemRenderer(itemBlockLamp, LampRenderer)

        lights.foreach(_.registerClient())
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

//        for (l <- lights) l.initClient()

//        MinecraftForgeClient.registerItemRenderer(itemPartIllumarButton, RenderButton)
//        MinecraftForgeClient.registerItemRenderer(itemPartIllumarFButton, RenderFButton)

//        MinecraftForgeClient.registerItemRenderer(Item.getItemFromBlock(ProjectRedIllumination.blockLamp), LampTESR)
        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileLamp], LampRenderer)

        MultiTileRenderRegistry.setRenderer(blockLamp, 0, LampRenderer)
    }

    var getLightValue = (meta:Int, brightness:Int) => brightness
}

object IlluminationProxy extends IlluminationProxy_client