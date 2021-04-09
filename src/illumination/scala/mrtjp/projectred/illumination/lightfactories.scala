package mrtjp.projectred.illumination

import codechicken.lib.raytracer.VoxelShapeCache
import codechicken.lib.render.CCModel
import codechicken.lib.texture.AtlasRegistrar
import codechicken.lib.vec._
import codechicken.multipart.api.part.TMultiPart
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.illumination.LightPartDefinition._
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.item.Item
import net.minecraft.util.ResourceLocation
import net.minecraft.util.math.shapes.VoxelShape

import scala.jdk.CollectionConverters._

object FixtureLightDefinition extends LightPartDefinition {
    private val bounds = sidedBoxes(new Cuboid6(3.5/16D, 0, 3.5/16D, 12.5/16D, 6.5/16D, 12.5/16D))
    private val glowBounds = sidedBoxes(new Cuboid6(4/16D, 1.5/16, 4/16D, 12/16D, 6.5/16D, 12/16D))

    private val shapes = bounds.map(VoxelShapeCache.getShape)

    private val bulbModels = new Array[CCModel](6)
    private val chassiModels = new Array[CCModel](6)
    private var icon:TextureAtlasSprite = _

    override protected def typeName:String = "fixture_light"

    override protected def itemFactory(colour:Int, inverted:Boolean):Item =
        new ItemBaseLight(this, colour, inverted)

    override protected def partFactory(colour:Int, inverted:Boolean):TMultiPart =
        new BaseLightFacePart(this, colour, inverted)

    override def getItemModelPath:String = "item/fixture_light"
    override def getItemModelLoaderPath:String = "fixture_light"

    override def registerIcons(registrar:AtlasRegistrar):Unit = {
        registrar.registerSprite(new ResourceLocation(ProjectRedIllumination.MOD_ID, "block/fixture"), icon = _)
    }

    override def loadModels():Unit = {
        val models = parseCorrectedModel("fixture")
        val chassi = models("chassi")
        val bulb = models("bulb")

        for (s <- 0 until 6) {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }

    override def getShape(side:Int):VoxelShape = shapes(side)
    override def getGlowBounds(side:Int):Cuboid6 = glowBounds(side)

    override def getIcon(colour:Int):TextureAtlasSprite = icon

    override def getModelBulb(side:Int):CCModel = bulbModels(side)
    override def getModelChassi(side:Int):CCModel = chassiModels(side)
}

object FalloutLightDefinition extends LightPartDefinition {
    private val bounds = sidedBoxes(new Cuboid6(2/16D, 0, 2/16D, 14/16D, 11/16D, 14/16D))
    private val glowBounds = sidedBoxes(new Cuboid6(4/16D, 1.5/16, 4/16D, 12/16D, 10/16D, 12/16D).expand(-0.002))

    private val shapes = bounds.map(VoxelShapeCache.getShape)

    private val bulbModels = new Array[CCModel](6)
    private val chassiModels = new Array[CCModel](6)
    private var icon:TextureAtlasSprite = _

    override protected def typeName:String = "fallout_light"

    override protected def itemFactory(colour:Int, inverted:Boolean):Item =
        new ItemBaseLight(this, colour, inverted)

    override protected def partFactory(colour:Int, inverted:Boolean):TMultiPart =
        new BaseLightFacePart(this, colour, inverted)

    override def getItemModelPath:String = "item/fallout_light"
    override def getItemModelLoaderPath:String = "fallout_light"

    override def registerIcons(registrar:AtlasRegistrar):Unit = {
        registrar.registerSprite(new ResourceLocation(ProjectRedIllumination.MOD_ID, "block/fallout"), icon = _)
    }

    override def loadModels():Unit = {
        val models = parseCorrectedModel("fallout")
        val chassi = models("chassi")
        val bulb = models("bulb")

        for (s <- 0 until 6) {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }

    override def getShape(side:Int):VoxelShape = shapes(side)
    override def getGlowBounds(side:Int):Cuboid6 = glowBounds(side)

    override def getIcon(colour:Int):TextureAtlasSprite = icon

    override def getModelBulb(side:Int):CCModel = bulbModels(side)
    override def getModelChassi(side:Int):CCModel = chassiModels(side)
}

object CageLightDefinition extends LightPartDefinition {
    private val bounds = sidedBoxes(new Cuboid6(3.5/16D, 0, 3.5/16D, 12.5/16D, 12/16D, 12.5/16D))
    private val glowBounds = sidedBoxes(new Cuboid6(4.5/16D, 1.5/16, 4.5/16D, 11.5/16D, 11.5/16D, 11.5/16D))

    private val shapes = bounds.map(VoxelShapeCache.getShape)

    private val bulbModels = new Array[CCModel](6)
    private val chassiModels = new Array[CCModel](6)
    private var icon:TextureAtlasSprite = _

    override protected def typeName:String = "cage_light"

    override protected def itemFactory(colour:Int, inverted:Boolean):Item =
        new ItemBaseLight(this, colour, inverted)

    override protected def partFactory(colour:Int, inverted:Boolean):TMultiPart =
        new BaseLightFacePart(this, colour, inverted)

    override def getItemModelPath:String = "item/cage_light"
    override def getItemModelLoaderPath:String = "cage_light"

    override def registerIcons(registrar:AtlasRegistrar):Unit = {
        registrar.registerSprite(new ResourceLocation(ProjectRedIllumination.MOD_ID, "block/cage_lamp"), icon = _)
    }

    override def loadModels():Unit = {
        val models = parseCorrectedModel("cagelamp")
        val chassi = models("chassi")
        val bulb = models("bulb")

        for (s <- 0 until 6) {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }

    override def getShape(side:Int):VoxelShape = shapes(side)
    override def getGlowBounds(side:Int):Cuboid6 = glowBounds(side)

    override def getIcon(colour:Int):TextureAtlasSprite = icon

    override def getModelBulb(side:Int):CCModel = bulbModels(side)
    override def getModelChassi(side:Int):CCModel = chassiModels(side)
}

object LanternLightDefinition extends LightPartDefinition {
    private val bounds = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D)
    private val shape = VoxelShapeCache.getShape(bounds)
    private val glowBounds = bounds.copy.expand(-1/64D)

    var icon:TextureAtlasSprite = _
    var bulbModel:CCModel = _
    val chassiModels = new Array[CCModel](7)

    override protected def typeName:String = "lantern"

    override protected def itemFactory(colour:Int, inverted:Boolean):Item =
        new ItemBaseLight(this, colour, inverted)

    override protected def partFactory(colour:Int, inverted:Boolean):TMultiPart =
        new BaseLightPart(this, colour, inverted)

    override def getItemModelPath:String = "item/lantern"
    override def getItemModelLoaderPath:String = "lantern"

    override def registerIcons(registrar:AtlasRegistrar):Unit = {
        registrar.registerSprite(new ResourceLocation(ProjectRedIllumination.MOD_ID, "block/lantern"), icon = _)
    }

    override def loadModels():Unit = {
        import LightPartDefinition._

        val models = parseCorrectedModel("lantern")

        val bulb = models("bulb")
        val body = models("body")
        val top = models("standtop")
        val topRing = models("goldringtop")
        val bottom = models("standbottom")
        val bottomRing = models("goldringbottom")
        val side = models("standside")

        bulbModel = bulb
        chassiModels(0) = CCModel.combine(Seq(body, bottom, bottomRing).asJava)
        chassiModels(1) = CCModel.combine(Seq(body, top, topRing).asJava)
        chassiModels(6) = CCModel.combine(Seq(body, topRing).asJava) //Inv model

        for (s <- 2 until 6) {
            val mSide = side.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.CENTER))
            val mRing = topRing.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.CENTER))
            chassiModels(s) = CCModel.combine(Seq(body, mSide, mRing).asJava)
        }

        chassiModels.foreach(finishModel)
        finishModel(bulbModel)
    }

    override def getShape(side:Int):VoxelShape = shape
    override def getGlowBounds(side:Int):Cuboid6 = glowBounds

    override def getIcon(colour:Int):TextureAtlasSprite = icon

    override def getModelBulb(side:Int):CCModel = bulbModel
    override def getModelChassi(side:Int):CCModel = chassiModels(side)

}
