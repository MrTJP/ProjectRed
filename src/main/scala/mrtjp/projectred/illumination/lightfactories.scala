package mrtjp.projectred.illumination

import codechicken.lib.render.CCModel
import codechicken.lib.vec._
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.util.{BlockRenderLayer, ResourceLocation}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.JavaConversions._

object LightFactoryFixture extends LightFactory
{
    val bounds = bakedBoxes(new Cuboid6(3.5/16D, 0, 3.5/16D, 12.5/16D, 6.5/16D, 12.5/16D))
    val lBounds = bakedBoxes(new Cuboid6(4/16D, 1.5/16, 4/16D, 12/16D, 6.5/16D, 12/16D))

    var icon:TextureAtlasSprite = _

    val bulbModels = new Array[CCModel](6)
    val chassiModels = new Array[CCModel](6)

    override def getUnlocalizedName(inv:Boolean) = if (inv) "fixtureLightInverted" else "fixtureLight"
    override def getItemRegistryName(inv:Boolean) = if (inv) "inverted_fixture_light" else "fixture_light"
    override def getType = new ResourceLocation("projectred-illumination:fixture_light")

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    override def getModelBulb(side:Int) = bulbModels(side)
    override def getModelChassi(side:Int) = chassiModels(side)

    override def getIcon = icon

    @SideOnly(Side.CLIENT)
    override def registerTextures(map:TextureMap)
    {
        icon = map.registerSprite(new ResourceLocation("projectred:blocks/lighting/fixture"))
    }

    override def loadModels()
    {
        val models = parseModel("fixture")
        val chassi = models.get("chassi")
        val bulb = models.get("bulb")

        for (s <- 0 until 6) {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }
}

object LightFactoryFallout extends LightFactory
{
    val bounds = bakedBoxes(new Cuboid6(2/16D, 0, 2/16D, 14/16D, 11/16D, 14/16D))
    val lBounds = bakedBoxes(new Cuboid6(4/16D, 1.5/16, 4/16D, 12/16D, 10/16D, 12/16D).expand(-0.002))

    var icon:TextureAtlasSprite = _

    val bulbModels = new Array[CCModel](6)
    val chassiModels = new Array[CCModel](6)


    override def getUnlocalizedName(inv:Boolean) = if (inv) "falloutLampInverted" else "falloutLamp"
    override def getItemRegistryName(inv:Boolean) = if (inv) "inverted_fallout_lamp" else "fallout_lamp"
    override def getType = new ResourceLocation("projectred-illumination:fallout_lamp")

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    override def getModelBulb(side:Int) = bulbModels(side)
    override def getModelChassi(side:Int) = chassiModels(side)

    override def getIcon = icon
    @SideOnly(Side.CLIENT)
    override def registerTextures(map:TextureMap)
    {
        icon = map.registerSprite(new ResourceLocation("projectred:blocks/lighting/fallout"))
    }

    override def loadModels()
    {
        val models = parseModel("fallout")
        val chassi = models.get("chassi")
        val bulb = models.get("bulb")

        for (s <- 0 until 6) {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }
}

object LightFactoryCage extends LightFactory
{
    val bounds = bakedBoxes(new Cuboid6(3.5/16D, 0, 3.5/16D, 12.5/16D, 12/16D, 12.5/16D))
    val lBounds = bakedBoxes(new Cuboid6(4.5/16D, 1.5/16, 4.5/16D, 11.5/16D, 11.5/16D, 11.5/16D))

    var icon:TextureAtlasSprite = _

    val bulbModels = new Array[CCModel](6)
    val chassiModels = new Array[CCModel](6)


    override def getUnlocalizedName(inv:Boolean) = if (inv) "cageLampInverted" else "cageLamp"
    override def getItemRegistryName(inv:Boolean) = if (inv) "inverted_cage_lamp" else "cage_lamp"
    override def getType = new ResourceLocation("projectred-illumination:cage_lamp")

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    override def getModelBulb(side:Int) = bulbModels(side)
    override def getModelChassi(side:Int) = chassiModels(side)

    @SideOnly(Side.CLIENT)
    override def getRenderLayer = BlockRenderLayer.CUTOUT

    override def getIcon = icon
    @SideOnly(Side.CLIENT)
    override def registerTextures(map:TextureMap)
    {
        icon = map.registerSprite(new ResourceLocation("projectred:blocks/lighting/cage_lamp"))
    }

    override def loadModels()
    {
        val models = parseModel("cagelamp")
        val chassi = models.get("chassi")
        val bulb = models.get("bulb")

        for (s <- 0 until 6) {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }
}

object LightFactoryLantern extends LightFactory
{
    private val bounds = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D)
    private val lBounds = bounds.copy.expand(-1/64D)

    var icon:TextureAtlasSprite = _

    var bulbModel:CCModel = _
    val chassiModels = new Array[CCModel](7)

    override def getUnlocalizedName(inv:Boolean) = if (inv) "lanternInverted" else "lantern"
    override def getItemRegistryName(inv:Boolean) = if (inv) "inverted_lantern" else "lantern"
    override def getType = new ResourceLocation("projectred-illumination:lantern")

    override def getBounds(side:Int) = bounds
    override def getLBounds(side:Int) = lBounds

    override def createPart = new BaseLightPart(this)

    override def getModelBulb(side:Int) = bulbModel
    override def getModelChassi(side:Int) = chassiModels(side)
    override def getInvModelChassi = chassiModels(6)

    override def getIcon = icon
    @SideOnly(Side.CLIENT)
    override def registerTextures(map:TextureMap)
    {
        icon = map.registerSprite(new ResourceLocation("projectred:blocks/lighting/lantern"))
    }

    override def getItemRenderTransform(t:TransformType) = t match {
        case TransformType.GUI =>
            val (_, rot, _) = super.getItemRenderTransform(t)
            (new Vector3(0, -1/16D, 0), rot, 1.25)
        case _ => super.getItemRenderTransform(t)
    }

    override def loadModels()
    {
        val models = parseModel("lantern")

        val bulb = models.get("bulb")
        val body = models.get("body")
        val top = models.get("standtop")
        val topRing = models.get("goldringtop")
        val bottom = models.get("standbottom")
        val bottomRing = models.get("goldringbottom")
        val side = models.get("standside")

        bulbModel = bulb
        chassiModels(0) = CCModel.combine(Seq(body, bottom, bottomRing))
        chassiModels(1) = CCModel.combine(Seq(body, top, topRing))
        chassiModels(6) = CCModel.combine(Seq(body, topRing)) //Inv model

        for (s <- 2 until 6) {
            val mSide = side.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.center))
            val mRing = topRing.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.center))
            chassiModels(s) = CCModel.combine(Seq(body, mSide, mRing))
        }

        chassiModels.foreach(finishModel)
        finishModel(bulbModel)
    }
}
