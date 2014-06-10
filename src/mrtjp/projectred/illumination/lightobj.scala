package mrtjp.projectred.illumination

import net.minecraftforge.client.IItemRenderer.ItemRenderType
import net.minecraft.client.renderer.texture.IIconRegister
import codechicken.lib.vec._
import cpw.mods.fml.relauncher.{Side, SideOnly}
import net.minecraft.util.{ResourceLocation, IIcon}
import codechicken.lib.render.{TextureUtils, CCRenderState, CCModel}
import mrtjp.projectred.core.InvertX
import codechicken.lib.lighting.LightModel
import codechicken.lib.render.uv.IconTransformation
import org.lwjgl.opengl.GL11
import net.minecraft.client.renderer.Tessellator
import scala.collection.JavaConversions._

trait TLightRenderHelper extends LightObject
{
    @SideOnly(Side.CLIENT)
    def prepairInvRender(x:Double, y:Double, z:Double, s:Double)
    {
        GL11.glPushMatrix()
        GL11.glTranslated(x, y, z)
        GL11.glScaled(s, s, s)
        CCRenderState.reset()
        TextureUtils.bindAtlas(0)
        CCRenderState.useNormals = true
        CCRenderState.startDrawing()
    }

    @SideOnly(Side.CLIENT)
    def doInvRender(){CCRenderState.draw()}

    @SideOnly(Side.CLIENT)
    def renderInvLightBox(color:Int, trans:Transformation)
    {
        RenderHalo.prepareRenderState()
        RenderHalo.renderHalo(Tessellator.instance, getLBounds(0), color, trans)
        RenderHalo.restoreRenderState()
    }

    @SideOnly(Side.CLIENT)
    def endInvRender(){GL11.glPopMatrix()}
}

object LightObjLantern extends LightObject with TLightRenderHelper
{
    private val bounds = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D)
    private val lBounds = bounds.copy.expand(-1/64D)

    val on = new Array[IIcon](16)
    val off = new Array[IIcon](16)

    val lModels = new Array[CCModel](7)

    {//Obj parsing
        val models = CCModel.parseObjModels(new ResourceLocation("projectred", "textures/obj/lights/lantern.obj"), 7, new InvertX)
        for ((s, c) <- models) c.apply(new Translation(0.5, 0, 0.5))

        val bulb = models.get("bulb")
        val top = models.get("standtop")
        val topRing = models.get("goldringtop")
        val bottom = models.get("standbottom")
        val bottomRing = models.get("goldringbottom")
        val side = models.get("standside")

        lModels(0) = CCModel.combine(Seq(bulb, bottom, bottomRing))
        lModels(1) = CCModel.combine(Seq(bulb, top, topRing))
        lModels(6) = CCModel.combine(Seq(bulb, topRing)) //Inv model

        for (s <- 2 until 6)
        {
            lModels(s) = CCModel.combine(Seq(
                side.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s))),
                topRing.copy.apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)))
            ))
        }

        for (c <- lModels)
        {
            c.computeLighting(LightModel.standardLightModel)
            c.shrinkUVs(0.0005)
        }
    }

    override def getItemName = "projectred.illumination.lantern"
    override def getType = "pr_lantern"

    override def getBounds(side:Int) = bounds
    override def getLBounds(side:Int) = lBounds

    override def createPart = new BaseLightPart(this)

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        for (i <- 0 until 16)
        {
            on(i) = reg.registerIcon("projectred:lights/1_on/"+i)
            off(i) = reg.registerIcon("projectred:lights/1_off/"+i)
        }
    }

    @SideOnly(Side.CLIENT)
    override def render(part:BaseLightPart, color:Int, isOn:Boolean, pos:Vector3)
    {
        val icon = new IconTransformation(if (isOn) on(color) else off(color))
        TextureUtils.bindAtlas(0)
        lModels(part.side).render(new Translation(pos), icon)
    }

    @SideOnly(Side.CLIENT)
    override def renderInv(color:Int, inverted:Boolean, t:ItemRenderType)
    {
        val icon = new IconTransformation(if (inverted) on(color) else off(color))
        import ItemRenderType._
        t match
        {
            case ENTITY => render(-0.25, 0, -0.25, 1)
            case EQUIPPED => render(-0.15, -0.15, -0.15, 2)
            case EQUIPPED_FIRST_PERSON => render(-0.15, -0.15, -0.15, 2)
            case INVENTORY => render(0, -0.05, 0, 2)
            case _ =>
        }

        def render(x:Double, y:Double, z:Double, scale:Double)
        {
            prepairInvRender(x, y, z, scale)

            val trans = new Translation(x, y, z)
            lModels(6).render(trans, icon)
            doInvRender()
            if (inverted) renderInvLightBox(color, trans)

            endInvRender()
        }
    }
}

object LightObjFixture extends LightObject with TLightRenderHelper
{
    private val lModels = new Array[CCModel](6)

    private val bounds = new Array[Cuboid6](6)
    private val lBounds = new Array[Cuboid6](6)

    {
        val models = CCModel.parseObjModels(new ResourceLocation("projectred", "textures/obj/lights/fixture.obj"), 7, new InvertX)
        for (s <- 0 until 6)
        {
            val m = models.get("base").copy
            m.apply(new Translation(0.5, 0, 0.5))
            m.apply(Rotation.sideOrientation(s, 0).at(Vector3.center))
            m.computeLighting(LightModel.standardLightModel)
            m.shrinkUVs(0.0005)
            lModels(s) = m

            val t = Rotation.sideRotations(s).at(Vector3.center)
            bounds(s) = new Cuboid6(2/16D, 0, 2/16D, 14/16D, 17/32D, 14/16D).apply(t)
            lBounds(s) = new Cuboid6(5/32D, 0, 5/32D, 27/32D, 17/32D, 27/32D).apply(t)
        }
    }

    override def getItemName = "projectred.illumination.fixture"
    override def getType = "pr_fixture"

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    @SideOnly(Side.CLIENT)
    override def render(part:BaseLightPart, color:Int, isOn:Boolean, pos:Vector3)
    {
        val icon = new IconTransformation(if (isOn) LightObjLantern.on(color) else LightObjLantern.off(color))
        TextureUtils.bindAtlas(0)
        lModels(part.side).render(new Translation(pos), icon)
    }

    @SideOnly(Side.CLIENT)
    override def renderInv(color:Int, inverted:Boolean, t:ItemRenderType)
    {
        val icon = new IconTransformation(if (inverted) LightObjLantern.on(color) else LightObjLantern.off(color))
        import ItemRenderType._
        t match
        {
            case ENTITY => render(-0.25D, 0D, -0.25D, 0.75D)
            case EQUIPPED => render(-0.15D, -0.15D, -0.15D, 1.5D)
            case EQUIPPED_FIRST_PERSON => render(-0.15D, -0.15D, -0.15D, 1.5D)
            case INVENTORY => render(0D, -0.05D, 0D, 1D)
            case _ =>
        }

        def render(x:Double, y:Double, z:Double, scale:Double)
        {
            prepairInvRender(x, y, z, scale)
            val t = new Translation(x, y, z)
            lModels(0).render(t, icon)
            doInvRender()
            if (inverted) renderInvLightBox(color, t)
            endInvRender()
        }
    }
}

object LightObjCage extends LightObject with TLightRenderHelper
{
    private val lModels = new Array[CCModel](6)

    private val bounds = new Array[Cuboid6](6)
    private val lBounds = new Array[Cuboid6](6)

    {
        val models = CCModel.parseObjModels(new ResourceLocation("projectred", "textures/obj/lights/cagelamp.obj"), 7, new InvertX)
        for (s <- 0 until 6)
        {
            val m = models.get("base").copy
            m.apply(new Translation(0.5, 0, 0.5))
            m.apply(Rotation.sideOrientation(s, 0).at(Vector3.center))
            m.computeLighting(LightModel.standardLightModel)
            m.shrinkUVs(0.0005)
            lModels(s) = m

            val t = Rotation.sideRotations(s).at(Vector3.center)
            bounds(s) = new Cuboid6(2/16D, 0, 2/16D, 14/16D, 11/16D, 14/16D).apply(t)
            lBounds(s) = new Cuboid6(4/16D, 0, 4/16D, 12/16D, 10/16D, 12/16D).apply(t)
        }
    }

    override def getItemName = "projectred.illumination.cagelamp"
    override def getType = "pr_cagelamp"

    override def getBounds(side:Int) = bounds(side)
    override def getLBounds(side:Int) = lBounds(side)

    @SideOnly(Side.CLIENT)
    override def render(part:BaseLightPart, color:Int, isOn:Boolean, pos:Vector3)
    {
        val icon = new IconTransformation(if (isOn) LightObjLantern.on(color) else LightObjLantern.off(color))
        TextureUtils.bindAtlas(0)
        lModels(part.side).render(new Translation(pos), icon)
    }

    @SideOnly(Side.CLIENT)
    override def renderInv(color:Int, inverted:Boolean, t:ItemRenderType)
    {
        val icon = new IconTransformation(if (inverted) LightObjLantern.on(color) else LightObjLantern.off(color))
        import ItemRenderType._
        t match
        {
            case ENTITY => render(-0.25D, 0D, -0.25D, 0.75D)
            case EQUIPPED => render(-0.15D, -0.15D, -0.15D, 1.5D)
            case EQUIPPED_FIRST_PERSON => render(-0.15D, -0.15D, -0.15D, 1.5D)
            case INVENTORY => render(0D, -0.05D, 0D, 1D)
            case _ =>
        }

        def render(x:Double, y:Double, z:Double, scale:Double)
        {
            prepairInvRender(x, y, z, scale)

            val trans = new Translation(x, y, z)
            lModels(5).render(trans, icon)
            doInvRender()
            if (inverted) renderInvLightBox(color, trans)

            endInvRender()
        }
    }
}