package mrtjp.projectred.illumination

import codechicken.lib.render.CCModel
import codechicken.lib.vec._
import cpw.mods.fml.relauncher.{Side, SideOnly}
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.util.IIcon

import scala.collection.JavaConversions._

object LightObjFixture extends LightObject
{
    //TODO to val
    var bounds = bakedBoxes(new Cuboid6(2/16D, 0, 2/16D, 14/16D, 17/32D, 14/16D))
    var lBounds = bakedBoxes(new Cuboid6(4/16D, 1/16, 4/16D, 12/16D, 6.5/16D, 12/16D))

    var icon:IIcon = _

    val bulbModels = new Array[CCModel](6)
    val chassiModels = new Array[CCModel](6)

    override def getItemName = "projectred.illumination.fixture"
    override def getType = "pr_fixture"

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    override def getModelBulb(side:Int) = bulbModels(side)
    override def getModelChassi(side:Int) = chassiModels(side)

    override def getIcon = icon
    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        icon = reg.registerIcon("projectred:lights/fixture")
    }

    override def loadModels()
    {
        val models = parseModel("fixture")
        val chassi = models.get("chassi")
        val bulb = models.get("bulb")

        for (s <- 0 until 6)
        {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }
}

object LightObjFallout extends LightObject
{
    val bounds = bakedBoxes(new Cuboid6(2/16D, 0, 2/16D, 14/16D, 11/16D, 14/16D))
    val lBounds = bakedBoxes(new Cuboid6(4/16D, 1/16, 4/16D, 12/16D, 10/16D, 12/16D).expand(-0.002))

    var icon:IIcon = _

    val bulbModels = new Array[CCModel](6)
    val chassiModels = new Array[CCModel](6)

    override def getItemName = "projectred.illumination.cagelamp"
    override def getType = "pr_cagelamp"

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    override def getModelBulb(side:Int) = bulbModels(side)
    override def getModelChassi(side:Int) = chassiModels(side)

    override def getIcon = icon
    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        icon = reg.registerIcon("projectred:lights/fallout")
    }

    override def loadModels()
    {
        val models = parseModel("fallout")
        val chassi = models.get("chassi")
        val bulb = models.get("bulb")

        for (s <- 0 until 6)
        {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }
}

object LightObjCage extends LightObject
{
    val bounds = bakedBoxes(new Cuboid6(3.5/16D, 0, 3.5/16D, 12.5/16D, 12/16D, 12.5/16D))
    val lBounds = bakedBoxes(new Cuboid6(4.5/16D, 1/16, 4.5/16D, 11.5/16D, 11.5/16D, 11.5/16D))

    var icon:IIcon = _

    val bulbModels = new Array[CCModel](6)
    val chassiModels = new Array[CCModel](6)

    override def getItemName = "projectred.illumination.cagelamp2"
    override def getType = "pr_cagelamp2"

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    override def getModelBulb(side:Int) = bulbModels(side)
    override def getModelChassi(side:Int) = chassiModels(side)

    override def getIcon = icon
    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        icon = reg.registerIcon("projectred:lights/cagelamp")
    }

    override def loadModels()
    {
        val models = parseModel("cagelamp")
        val chassi = models.get("chassi")
        val bulb = models.get("bulb")

        for (s <- 0 until 6)
        {
            bulbModels(s) = bakeCopy(s, bulb)
            chassiModels(s) = bakeCopy(s, chassi)
        }
    }
}

object LightObjLantern extends LightObject
{
    private val bounds = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D)
    private val lBounds = bounds.copy.expand(-1/64D)

    var icon:IIcon = _

    var bulbModel:CCModel = _
    val chassiModels = new Array[CCModel](7)

    override def getItemName = "projectred.illumination.lantern"
    override def getType = "pr_lantern"

    override def getBounds(side:Int) = bounds
    override def getLBounds(side:Int) = lBounds

    override def createPart = new BaseLightPart(this)

    override def getModelBulb(side:Int) = bulbModel
    override def getModelChassi(side:Int) = chassiModels(side)
    override def getInvModelChassi = chassiModels(6)

    override def getIcon = icon
    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        icon = reg.registerIcon("projectred:lights/lantern")
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

        for (s <- 2 until 6)
        {
            val mSide = side.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.center))
            val mRing = topRing.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.center))
            chassiModels(s) = CCModel.combine(Seq(body, mSide, mRing))
        }

        chassiModels.foreach(finishModel)
        finishModel(bulbModel)
    }
}