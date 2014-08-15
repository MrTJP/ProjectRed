package mrtjp.projectred.core.libmc.gui

import mrtjp.projectred.core.libmc.{PRColors, ItemKeyStack}
import codechicken.lib.render.FontUtils
import java.util.{List => JList}
import net.minecraft.client.gui.Gui
import org.lwjgl.opengl.{GL12, GL11}
import net.minecraft.client.renderer.{OpenGlHelper, RenderHelper}
import net.minecraft.item.ItemStack
import net.minecraft.client.renderer.entity.RenderItem
import codechicken.lib.gui.GuiDraw

class WidgetItemList(x:Int, y:Int, w:Int, h:Int) extends TWidget
{
    override val bounds = new Rect().setMin(x, y).setWH(w, h)

    private val squareSize = 20
    private val rows = bHeight/squareSize
    private val columns = bWidth/squareSize

    private var currentPage = 0
    private var pagesNeeded = 0

    private var waitingForList = true
    private var downloadFinished = false

    private var selection:ItemKeyStack = null
    private var hover:ItemKeyStack = null

    private var xLast = 0
    private var yLast = 0

    private var filter = ""

    def getSelected = selection

    private var displayList = Vector[ItemKeyStack]()

    def setDisplayList(list:Vector[ItemKeyStack]):this.type =
    {
        displayList = list
        waitingForList = false
        currentPage = 0
        this
    }

    def setNewFilter(filt:String):this.type =
    {
        filter = filt.toLowerCase
        xLast = -1
        yLast = -1
        currentPage = 0
        this
    }

    def pageUp()
    {
        currentPage += 1
        if (currentPage > pagesNeeded) currentPage = pagesNeeded
    }

    def pageDown()
    {
        currentPage -= 1
        if (currentPage < 0) currentPage = 0
    }

    def resetDownloadStats()
    {
        waitingForList = true
        downloadFinished = false
    }

    def filterAllows(stack:ItemKeyStack):Boolean =
    {
        def stringMatch(name:String, filter:String):Boolean =
        {
            for (s <- filter.split(" ")) if (!name.contains(s)) return false
            true
        }

        if (stringMatch(stack.key.getName.toLowerCase, filter)) true
        else false
    }

    private def getSeachedCount =
    {
        var count = 0
        for (stack <- displayList) if (filterAllows(stack)) count += 1
        count
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        drawGradientRect(x, y, x+bWidth, y+bHeight, 0xff808080, 0xff808080)
        pagesNeeded = (getSeachedCount-1)/(rows*columns)
        if (pagesNeeded < 0) pagesNeeded = 0
        if (currentPage > pagesNeeded) currentPage = pagesNeeded

        if (!downloadFinished) drawLoadingScreen()
        else drawAllItems(mouse.x, mouse.y)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        if (hover != null) GuiDraw.drawMultilineTip(
            mouse.x+12, mouse.y-12,
            hover.makeStack.getTooltip(mcInst.thePlayer,
                mcInst.gameSettings.advancedItemTooltips).asInstanceOf[JList[String]])
        FontUtils.drawCenteredString(
            "Page: "+(currentPage+1)+"/"+(pagesNeeded+1), x+(bWidth/2), y+bHeight+6, PRColors.BLACK.rgb)
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (!consumed && bounds.intersects(p))
        {
            xLast = p.x
            yLast = p.y
            true
        }
        else false
    }

    private def drawLoadingScreen()
    {
        val barSizeX = bWidth/2
        val time = System.currentTimeMillis/(if (waitingForList) 40 else 8)
        val percent = (time%barSizeX).asInstanceOf[Int]

        if (!waitingForList && percent > barSizeX-8) downloadFinished = true
        val xStart = x+bWidth/2-barSizeX/2
        val yStart = y+bHeight/3

        FontUtils.drawCenteredString("downloading data", (x+bWidth)/2, (y+bHeight)/3+squareSize, 0xff165571)
        val xSize = percent
        val ySize = 9

        Gui.drawRect(xStart, yStart, xStart+xSize, yStart+ySize, 0xff165571)
    }

    private def drawAllItems(mx:Int, my:Int)
    {
        hover = null
        selection = null
        val xOffset = x-(squareSize-2)
        val yOffset = y+2
        var renderPointerX = 1
        var renderPointerY = 0
        var itemNumber = 0

        glItemPre()

        var b, c = new scala.util.control.Breaks
        b.breakable
        {
            for (keystack <- displayList) c.breakable
            {
                if (!filterAllows(keystack)) c.break()
                itemNumber += 1
                if (itemNumber <= rows*columns*currentPage) c.break()
                if (itemNumber > (rows*columns)*(currentPage+1)) b.break()

                val localX = xOffset+renderPointerX*squareSize
                val localY = yOffset+renderPointerY*squareSize
                if (mx > localX && mx < localX+squareSize && my > localY && my < localY+squareSize) hover = keystack
                if (xLast > localX && xLast < localX+squareSize && yLast > localY && yLast < localY+squareSize) selection = keystack

                if (selection != null && (selection == keystack))
                {
                    Gui.drawRect(localX-2, localY-2, localX+squareSize-2, localY+squareSize-2, 0xff000000)
                    Gui.drawRect(localX-1, localY-1, localX+squareSize-3, localY+squareSize-3, 0xffd2d2d2)
                    Gui.drawRect(localX, localY, localX+squareSize-4, localY+squareSize-4, 0xff595959)
                }

                inscribeItemStack(localX, localY, keystack.makeStack)
                renderPointerX += 1

                if (renderPointerX > columns)
                {
                    renderPointerX = 1
                    renderPointerY += 1
                }
                if (renderPointerY > rows) b.break()
            }
        }
        glItemPost()
    }

    private def glItemPre()
    {
        GL11.glPushMatrix()
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F)
        RenderHelper.enableGUIStandardItemLighting()
        GL11.glEnable(GL12.GL_RESCALE_NORMAL)
        OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, 240/1.0F, 240/1.0F)
        GL11.glDisable(GL11.GL_DEPTH_TEST)
        GL11.glDisable(GL11.GL_LIGHTING)
    }

    private def glItemPost()
    {
        GL11.glEnable(GL11.GL_DEPTH_TEST)
        GL11.glPopMatrix()
    }

    protected var renderItem = new RenderItem
    private def inscribeItemStack(xPos:Int, yPos:Int, stack:ItemStack)
    {
        val font = stack.getItem.getFontRenderer(stack) match
        {
            case null => fontRenderer
            case r => r
        }

        renderItem.zLevel = 100.0F
        GL11.glEnable(GL11.GL_DEPTH_TEST)
        GL11.glEnable(GL11.GL_LIGHTING)
        renderItem.renderItemAndEffectIntoGUI(font, renderEngine, stack, xPos, yPos)
        renderItem.renderItemOverlayIntoGUI(font, renderEngine, stack, xPos, yPos, "")
        GL11.glDisable(GL11.GL_LIGHTING)
        GL11.glDisable(GL11.GL_DEPTH_TEST)
        renderItem.zLevel = 0.0F

        var s:String = null
        if (stack.stackSize == 1) s = ""
        else if (stack.stackSize < 1000) s = stack.stackSize+""
        else if (stack.stackSize < 100000) s = stack.stackSize/1000+"K"
        else if (stack.stackSize < 1000000) s = "0M"+stack.stackSize/100000
        else s = stack.stackSize/1000000+"M"
        font.drawStringWithShadow(s, xPos+19-2-font.getStringWidth(s), yPos+6+3, 16777215)
    }
}
