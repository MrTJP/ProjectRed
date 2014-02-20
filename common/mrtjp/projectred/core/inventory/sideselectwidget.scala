package mrtjp.projectred.core.inventory

import codechicken.core.gui.GuiDraw
import codechicken.lib.math.MathHelper
import codechicken.lib.render.CCModel
import codechicken.lib.render.Vertex5
import codechicken.lib.vec.Rotation
import mrtjp.projectred.core.PRColors
import mrtjp.projectred.core.utils.DirectionalRayTracer
import mrtjp.projectred.core.utils.DirectionalRayTracer.HitCoord
import mrtjp.projectred.core.utils.MathLib
import net.minecraft.block.Block
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.client.renderer.Tessellator
import net.minecraft.client.renderer.texture.TextureMap
import net.minecraft.client.renderer.tileentity.TileEntityRenderer
import net.minecraft.tileentity.TileEntity
import net.minecraftforge.common.ForgeDirection
import org.lwjgl.input.Mouse
import org.lwjgl.opengl.GL11
import org.lwjgl.util.vector.Matrix4f
import org.lwjgl.util.vector.Vector3f
import scala.collection.JavaConversions
import scala.collection.mutable.ListBuffer

class WidgetSideSelect(x:Int, y:Int, width:Int, height:Int, scale:Double) extends GhostWidget(x, y, width, height)
{
    private val rotHook = new ClickRotation.ClickRotationHook(0, 40)

    private var init = false

    private var lastSideHovered = -1
    private var ticksHeld = 0

    var sides = 0

    override def drawBack(mouseX:Int, mouseY:Int, frame:Float)
    {
        super.drawBack(mouseX, mouseY, frame)

        if (!init || Mouse.isButtonDown(2))
        {
            rotHook.setTransform(getInitialRot)
            init = true
        }

        GL11.glPushMatrix()
        GL11.glTranslated(x+(scale/2), y+(scale/2), scale)
        GL11.glScaled(scale, -scale, scale)
        rotHook.update(mouseX-width, -(mouseY-height), pointInside(mouseX, mouseY))

        draw()

        val coord = new DirectionalRayTracer(0.5).getNearestHit
        if (coord != null) onSideHitByCursor(coord)

        lastSideHovered = if (coord!=null && coord.side!=ForgeDirection.UNKNOWN) coord.side.ordinal() else -1
        GL11.glPopMatrix()
    }

    override def drawFront(mouseX:Int, mouseY:Int)
    {
        super.drawFront(mouseX, mouseY)

        val list = ListBuffer[String]()
        buildToolTip(list, lastSideHovered)
        GuiDraw.drawMultilineTip(mouseX + 12, mouseY - 12, JavaConversions.bufferAsJavaList(list))
    }

    override def mouseClicked(x:Int, y:Int, button:Int)
    {
        ticksHeld = 0
        lastSideHovered = -1
    }

    override def mouseMovedOrUp(x:Int, y:Int, button:Int)
    {
        if (ticksHeld < 5 && lastSideHovered >= 0)
        {
            onSideClicked(lastSideHovered, button)
            ticksHeld = 5
        }
    }

    override def mouseDragged(x:Int, y:Int, button:Int, time:Long)
    {
        ticksHeld += 1
    }

    def getInitialRot = MathLib.createEntityRotateMatrix(mc.renderViewEntity)
    def onSideHitByCursor(hit:HitCoord) {}
    def draw() {}
    def onSideClicked(s:Int, button:Int) {}
    def buildToolTip(list:ListBuffer[String], hoverSide:Int) {}
}

trait TWidgetSidePicker extends WidgetSideSelect
{
    private var exclusiveSide:Boolean = false

    def setExclusive(flag:Boolean) =
    {
        exclusiveSide = flag
        this
    }

    abstract override def onSideClicked(s:Int, button:Int)
    {
        super.onSideClicked(s, button)
        toggleSide(s)
    }

    def toggleSide(side:Int)
    {
        val old:Int = sides
        sides ^= 1<<side
        if (exclusiveSide) sides &= 1<<side
        if (old != sides) onSideChanged(side)
    }

    def clearSides()
    {
        val old:Int = sides
        sides = 0
        if (old != sides) onSideChanged(-1)
    }

    def sideMask = sides

    def setSideMask(mask:Int) =
    {
        sides = mask
        this
    }

    def onSideChanged(s:Int) {}
}

trait TWidgetSideHighlight extends WidgetSideSelect
{
    private var color = PRColors.LIME.rgb
    private var activeHighlight = false

    def setColor(c:Int) =
    {
        color = c
        this
    }

    def setSideHighlighting(flag:Boolean) =
    {
        activeHighlight = flag
        this
    }

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
        val t = Tessellator.instance

        GL11.glDisable(GL11.GL_LIGHTING)
        GL11.glEnable(GL11.GL_BLEND)
        GL11.glDisable(GL11.GL_DEPTH_TEST)
        GL11.glDisable(GL11.GL_TEXTURE_2D)
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
        t.startDrawingQuads()
        t.setColorRGBA_I(color, 64)
        TWidgetSideHighlight.highlights(side).render()
        t.draw
        GL11.glEnable(GL11.GL_DEPTH_TEST)
        GL11.glEnable(GL11.GL_TEXTURE_2D)
        GL11.glDisable(GL11.GL_BLEND)
    }
}

object TWidgetSideHighlight
{
    private def genHighlightModel()
    {
        val model:CCModel = CCModel.quadModel(4)
        model.verts(0) = new Vertex5(-0.5, -0.5, -0.5, 0, 0)
        model.verts(1) = new Vertex5(0.5, -0.5, -0.5, 0, 0)
        model.verts(2) = new Vertex5(0.5, -0.5, 0.5, 0, 0)
        model.verts(3) = new Vertex5(-0.5, -0.5, 0.5, 0, 0)
        highlights = new Array[CCModel](6)
        highlights(0) = model

        for (s <- 1 until 6)
            highlights(s) = model.copy.apply(Rotation.sideRotations(s))
    }

    var highlights:Array[CCModel] = _
    genHighlightModel()

    val renderBlocks = new RenderBlocks
}

trait TWidgetSideBlock extends WidgetSideSelect
{
    var block:Block = null
    var meta = 0

    def setBlock(b:Block, md:Int) =
    {
        block = b
        meta = md
        this
    }

    abstract override def draw()
    {
        super.draw()

        val t = Tessellator.instance
        val renderBlocks = TWidgetSideBlock.renderBlocks

        GL11.glColor4f(1, 1, 1, 1)
        renderEngine.bindTexture(TextureMap.locationBlocksTexture)
        renderBlocks.setRenderBounds(0, 0, 0, 1, 1, 1)
        t.startDrawingQuads()
        renderBlocks.renderFaceXNeg(Block.stone, -0.5, -0.5, -0.5, block.getIcon(4, meta))
        renderBlocks.renderFaceXPos(Block.stone, -0.5, -0.5, -0.5, block.getIcon(5, meta))
        renderBlocks.renderFaceYPos(Block.stone, -0.5, -0.5, -0.5, block.getIcon(1, meta))
        renderBlocks.renderFaceYNeg(Block.stone, -0.5, -0.5, -0.5, block.getIcon(0, meta))
        renderBlocks.renderFaceZNeg(Block.stone, -0.5, -0.5, -0.5, block.getIcon(2, meta))
        renderBlocks.renderFaceZPos(Block.stone, -0.5, -0.5, -0.5, block.getIcon(3, meta))
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

    def setTile(te:TileEntity) =
    {
        tile = te
        this
    }

    abstract override def draw()
    {
        super.draw()
        TileEntityRenderer.instance.renderTileEntityAt(tile, -0.5, -0.5, -0.5, 0)
    }
}

trait TWidgetSideStaticRotation extends WidgetSideSelect
{
    override def getInitialRot =
    {
        val initial = new Matrix4f
        initial.rotate((20 * MathHelper.torad).asInstanceOf[Float], new Vector3f(1, 0, 0))
        initial.rotate((30 * MathHelper.torad).asInstanceOf[Float], new Vector3f(0, 1, 0))
        initial
    }
}

class JWidgetSideSelect(x:Int, y:Int, width:Int, height:Int, scale:Double)
    extends WidgetSideSelect(x, y, width, height, scale) with TWidgetSideTE with TWidgetSideHighlight with TWidgetSidePicker