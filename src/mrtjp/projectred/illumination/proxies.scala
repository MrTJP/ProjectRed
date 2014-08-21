package mrtjp.projectred.illumination

import codechicken.multipart.MultiPartRegistry
import mrtjp.projectred.ProjectRedIllumination._
import mrtjp.projectred.core.{RenderHalo, IProxy}
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.client.MinecraftForgeClient
import mrtjp.projectred.ProjectRedIllumination
import cpw.mods.fml.client.registry.ClientRegistry
import net.minecraftforge.common.MinecraftForge
import net.minecraft.item.Item

class IlluminationProxy_server extends IProxy with IPartFactory
{
    val lights = Seq(LightObjLantern, LightObjFixture, LightObjCage)

    override def preinit(){}

    override def init()
    {
        MultiPartRegistry.registerParts(this, (lights.map(_.getType) :+ "pr_lightbutton" :+ "pr_flightbutton").toArray)
        for (l <- lights) l.initServer()

        itemPartIllumarButton = new ItemPartButton
        itemPartIllumarFButton = new ItemPartFButton

        blockLamp = new BlockLamp
        blockLamp.addSingleTile(classOf[TileLamp])

        blockAirousLight = new BlockAirousLight
        blockAirousLight.bindTile(classOf[TileAirousLight])
    }

    override def postinit()
    {
        IlluminationRecipes.initRecipes()
    }

    override def createPart(name:String, client:Boolean) = name match
    {
        case "pr_lightbutton" => new LightButtonPart
        case "pr_flightbutton" => new FLightButtonPart
        case _ => getLight(name)
    }

    private def getLight(name:String) = lights.find(_.getType == name) match
    {
        case Some(e) => e.createPart
        case None => null
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class IlluminationProxy_client extends IlluminationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

        for (l <- lights) l.initClient()

        MinecraftForgeClient.registerItemRenderer(itemPartIllumarButton, RenderButton)
        MinecraftForgeClient.registerItemRenderer(itemPartIllumarFButton, RenderFButton)

        MinecraftForgeClient.registerItemRenderer(Item.getItemFromBlock(ProjectRedIllumination.blockLamp), LampTESR)
        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileLamp], LampTESR)
    }
}

object IlluminationProxy extends IlluminationProxy_client