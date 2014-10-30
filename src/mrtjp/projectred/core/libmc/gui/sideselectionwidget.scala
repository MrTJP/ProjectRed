package mrtjp.projectred.core.libmc.gui

import codechicken.lib.gui.GuiDraw
import codechicken.lib.math.MathHelper
import codechicken.lib.render.{CCModel, CCRenderState, ColourMultiplier, Vertex5}
import codechicken.lib.vec.Rotation
import com.google.common.base.Preconditions
import mrtjp.core.color.Colors
import mrtjp.core.gui.TWidget
import mrtjp.core.math.MathLib
import mrtjp.core.vec.{Point, Rect}
import mrtjp.projectred.core.libmc.DirectionalRayTracer.HitCoord
import mrtjp.projectred.core.libmc.{DirectionalRayTracer, OpenGLLib}
import net.minecraft.block.Block
import net.minecraft.client.renderer.texture.TextureMap
import net.minecraft.client.renderer.tileentity.TileEntityRendererDispatcher
import net.minecraft.client.renderer.{RenderBlocks, Tessellator}
import net.minecraft.init.Blocks
import net.minecraft.tileentity.TileEntity
import net.minecraftforge.common.util.ForgeDirection
import org.lwjgl.input.Mouse
import org.lwjgl.opengl.GL11
import org.lwjgl.util.vector.{Matrix4f, Vector3f}

import scala.collection.JavaConversions
import scala.collection.mutable.ListBuffer

class WidgetSideSelect(x:Int, y:Int, w:Int, h:Int, scale:Double) extends TWidget
{
    override val bounds = new Rect().setMin(x, y).setWH(w, h)

    private val rotHook = new SphereRotationHook(0, 40)

    private var init = false
    private var lastSideHovered = -1
    private var ticksHeld = 0
    var sides = 0

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        if (!init || Mouse.isButtonDown(2))
        {
            rotHook.setTransform(getInitialRot)
            init = true
        }

        GL11.glPushMatrix()
        GL11.glTranslated(x+(scale/2), y+(scale/2), scale)
        GL11.glScaled(scale, -scale, scale)
        rotHook.update(mouse.x-bWidth, -(mouse.y-bHeight), bounds.intersects(mouse))

        draw()

        val coord = new DirectionalRayTracer(0.5).getNearestHit
        if (coord != null) onSideHitByCursor(coord)

        lastSideHovered = if (coord!=null && coord.side!=ForgeDirection.UNKNOWN) coord.side.ordinal() else -1
        GL11.glPopMatrix()
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        val list = ListBuffer[String]()
        buildToolTip(list, lastSideHovered)
        GuiDraw.drawMultilineTip(mouse.x+12, mouse.y-12, JavaConversions.bufferAsJavaList(list))
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        ticksHeld = 0
        lastSideHovered = -1
        bounds.intersects(p)
    }

    override def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (ticksHeld < 5 && lastSideHovered >= 0)
        {
            onSideClicked(lastSideHovered, button)
            ticksHeld = 5
        }
        true
    }

    override def mouseDragged_Impl(p:Point, button:Int, time:Long, consumed:Boolean) =
    {
        ticksHeld += 1
        true
    }

    def getInitialRot = MathLib.createEntityRotateMatrix(mcInst.renderViewEntity)
    def onSideHitByCursor(hit:HitCoord){}
    def draw(){}
    def onSideClicked(s:Int, button:Int){}
    def buildToolTip(list:ListBuffer[String], hoverSide:Int){}
}

trait TWidgetSidePicker extends WidgetSideSelect
{
    private var exclusiveSide = false
    def setExclusive(flag:Boolean):this.type = {exclusiveSide = flag; this}

    abstract override def onSideClicked(s:Int, button:Int)
    {
        super.onSideClicked(s, button)
        toggleSide(s)
    }

    def toggleSide(side:Int)
    {
        val old = sides
        sides ^= 1<<side
        if (exclusiveSide) sides &= 1<<side
        if (old != sides) onSideChanged(side)
    }

    def clearSides()
    {
        val old = sides
        sides = 0
        if (old != sides) onSideChanged(-1)
    }

    def sideMask = sides

    def setSideMask(mask:Int):this.type = {sides = mask; this}

    def onSideChanged(s:Int){}
}

trait TWidgetSideHighlight extends WidgetSideSelect
{
    private var color = Colors.LIME.rgba
    private var activeHighlight = false

    def setColor(c:Int):this.type = {color = c; this}

    def setSideHighlighting(flag:Boolean):this.type = {activeHighlight = flag; this}

    abstract override def onSideHitByCursor(hit:HitCoord)
    {
        super.onSideHitByCursor(hit)
        renderHighlight(hit.side.ordinal())
    }

    abstract override def draw()
    {
        super.draw()
        if (activeHighlight)
            for (s <- 0 until 6) if ((sides&1<<s) != 0) renderHighlight(s)
    }

    private def renderHighlight(side:Int)
    {
        GL11.glDisable(GL11.GL_LIGHTING)
        GL11.glEnable(GL11.GL_BLEND)
        GL11.glDisable(GL11.GL_DEPTH_TEST)
        GL11.glDisable(GL11.GL_TEXTURE_2D)
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

        CCRenderState.reset()
        CCRenderState.pullLightmap()
        CCRenderState.setDynamic()
        CCRenderState.baseColour = color
        CCRenderState.alphaOverride = 64
        CCRenderState.startDrawing()
        TWidgetSideHighlight.highlights(side).render(ColourMultiplier.instance(color))
        CCRenderState.draw()

        GL11.glEnable(GL11.GL_DEPTH_TEST)
        GL11.glEnable(GL11.GL_TEXTURE_2D)
        GL11.glDisable(GL11.GL_BLEND)
    }
}

object TWidgetSideHighlight
{
    private def genHighlightModel()
    {
        val model = CCModel.quadModel(4)
        model.verts(0) = new Vertex5(-0.5, -0.5, -0.5, 0, 0)
        model.verts(1) = new Vertex5(0.5, -0.5, -0.5, 0, 0)
        model.verts(2) = new Vertex5(0.5, -0.5, 0.5, 0, 0)
        model.verts(3) = new Vertex5(-0.5, -0.5, 0.5, 0, 0)
        highlights = new Array[CCModel](6)
        highlights(0) = model

        for (s <- 1 until 6)
            highlights(s) = model.copy.apply(Rotation.sideRotations(s))

        highlights.foreach(_.computeNormals())
    }

    var highlights:Array[CCModel] = _
    genHighlightModel()

    val renderBlocks = new RenderBlocks
}

trait TWidgetSideBlock extends WidgetSideSelect
{
    var block:Block = null
    var meta = 0

    def setBlock(b:Block, md:Int):this.type = {block = b; meta = md; this}

    abstract override def draw()
    {
        super.draw()

        val t = Tessellator.instance
        val renderBlocks = TWidgetSideBlock.renderBlocks

        GL11.glColor4f(1, 1, 1, 1)
        renderEngine.bindTexture(TextureMap.locationBlocksTexture)
        renderBlocks.setRenderBounds(0, 0, 0, 1, 1, 1)
        t.startDrawingQuads()
        renderBlocks.renderFaceXNeg(Blocks.stone, -0.5, -0.5, -0.5, block.getIcon(4, meta))
        renderBlocks.renderFaceXPos(Blocks.stone, -0.5, -0.5, -0.5, block.getIcon(5, meta))
        renderBlocks.renderFaceYPos(Blocks.stone, -0.5, -0.5, -0.5, block.getIcon(1, meta))
        renderBlocks.renderFaceYNeg(Blocks.stone, -0.5, -0.5, -0.5, block.getIcon(0, meta))
        renderBlocks.renderFaceZNeg(Blocks.stone, -0.5, -0.5, -0.5, block.getIcon(2, meta))
        renderBlocks.renderFaceZPos(Blocks.stone, -0.5, -0.5, -0.5, block.getIcon(3, meta))
        t.draw
    }
}

object TWidgetSideBlock
{
    val renderBlocks = new RenderBlocks
}

trait TWidgetSideTE extends WidgetSideSelect
{
    var tile:TileEntity = null

    def setTile(te:TileEntity):this.type = {tile = te; this}

    abstract override def draw()
    {
        super.draw()
        TileEntityRendererDispatcher.instance.renderTileEntityAt(tile, -0.5, -0.5, -0.5, 0)
    }
}

trait TWidgetSideStaticRotation extends WidgetSideSelect
{
    override def getInitialRot =
    {
        val initial = new Matrix4f
        initial.rotate((20*MathHelper.torad).asInstanceOf[Float], new Vector3f(1, 0, 0))
        initial.rotate((30*MathHelper.torad).asInstanceOf[Float], new Vector3f(0, 1, 0))
        initial
    }
}

class JWidgetSideSelect(x:Int, y:Int, width:Int, height:Int, scale:Double)
    extends WidgetSideSelect(x, y, width, height, scale) with TWidgetSideTE with TWidgetSideHighlight with TWidgetSidePicker

class SphericalRotation
{
    private var dragStart:Vector3f = null
    private var lastTransform = new Matrix4f

    def setTransformation(t:Matrix4f){lastTransform = t}

    private def calculateSpherePoint(x:Float, y:Float) =
    {
        val result = new Vector3f(x, y, 0)
        val sqrZ = 1-Vector3f.dot(result, result)
        if (sqrZ > 0) result.z = Math.sqrt(sqrZ).asInstanceOf[Float]
        else result.normalise()
        result
    }

    private def getTransform(mouseX:Float, mouseY:Float):Matrix4f =
    {
        Preconditions.checkNotNull(dragStart, "", "Draging not started")
        val current = calculateSpherePoint(mouseX, mouseY)
        val dot = Vector3f.dot(dragStart, current)
        if (Math.abs(dot - 1) < 0.0001) return lastTransform

        val axis = Vector3f.cross(dragStart, current, null)
        axis.normalise()

        val angle = 2*Math.acos(dot).asInstanceOf[Float]
        val rotation = new Matrix4f
        rotation.rotate(angle, axis)
        Matrix4f.mul(rotation, lastTransform, null)
    }

    def applyTransform(mouseX:Float, mouseY:Float, isDragging:Boolean)
    {
        OpenGLLib.loadMatrix(if (isDragging) getTransform(mouseX, mouseY) else lastTransform)
    }

    def startDrag(mouseX:Float, mouseY:Float){dragStart = calculateSpherePoint(mouseX, mouseY)}

    def endDrag(mouseX:Float, mouseY:Float){lastTransform = getTransform(mouseX, mouseY)}
}

class SphereRotationHook(mouseButton:Int, radius:Int)
{
    private val target = new SphericalRotation
    private var isDragging = false

    def update(mouseX:Int, mouseY:Int, drag:Boolean)
    {
        val mx = mouseX/radius
        val my = mouseY/radius
        val buttonState = Mouse.isButtonDown(mouseButton)

        if (!isDragging && buttonState && drag)
        {
            isDragging = true
            target.startDrag(mx, my)
        }
        else if (isDragging && !buttonState)
        {
            isDragging = false
            target.endDrag(mx, my)
        }
        target.applyTransform(mx, my, isDragging)
    }

    def setTransform(t:Matrix4f){target.setTransformation(t)}
}