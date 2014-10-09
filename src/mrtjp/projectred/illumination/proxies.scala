package mrtjp.projectred.illumination

import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.client.registry.ClientRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.ProjectRedIllumination._
import mrtjp.projectred.core.{Configurator, IProxy}
import mrtjp.projectred.core.libmc.PRColors
import net.minecraft.item.Item
import net.minecraftforge.client.MinecraftForgeClient

class IlluminationProxy_server extends IProxy with IPartFactory
{
    val lights = Seq(LightObjLantern, LightObjFixture, LightObjFallout, LightObjCage)

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

        IlluminationRecipes.initRecipes()
    }

    override def postinit(){}

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


    /**
     * Copied from ColoredLights API
     */
    /**
     * Computes a 20-bit lighting word, containing red, green, blue, and brightness settings.
     * Allows overriding of the Minecraft brightness value.
     * This value can be used directly for Block.lightValue
     *
     * Word format: 0RRRR 0GGGG 0BBBB 0LLLL
     *
     * @param r1 Red intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param g1 Green intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param b1 Blue intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param brightness1 The existing lightValue of a block. Only the lower-most 4 bits of this parameter are used.
     * @return Integer describing RGBL color for a block
     */
    def makeRGBLightValue(r1:Float, g1:Float, b1:Float, brightness1:Float):Int =
    {
        var (r, g, b) = (r1, g1, b1)

        if (r < 0.0f) r = 0.0f
        else if (r > 1.0f) r = 1.0f

        if (g < 0.0f) g = 0.0f
        else if (g > 1.0f) g = 1.0f

        if (b < 0.0f) b = 0.0f
        else if (b > 1.0f) b = 1.0f

        var brightness = (brightness1*15.0f).asInstanceOf[Int]
        brightness &= 15

        brightness|(((15.0F*b).asInstanceOf[Int]<<15)+((15.0F*g).asInstanceOf[Int]<<10)+((15.0F*r).asInstanceOf[Int]<<5))
    }

    val cache =
    {
        val b = Vector.newBuilder[Int]
        for (i <- 0 until 15)
        {
            val c = PRColors.get(i)
            b += makeRGBLightValue(c.rF, c.gF, c.bF, 1.0F)
        }
        b.result()
    }
    def makeRGBLightValue(meta:Int, lValue:Int):Int =
    {
        if (!Configurator.coloredLightsCompat) return lValue
        if (lValue >= 15) return cache(meta)
        val color = PRColors.get(meta)
        import color.{bF, gF, rF}
        makeRGBLightValue(rF, gF, bF, lValue/15.0F)
    }
}

object IlluminationProxy extends IlluminationProxy_client