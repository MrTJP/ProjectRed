/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.vec.{Point, Rect, Size}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.Gui
import net.minecraft.entity.player.EntityPlayer
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11

import scala.collection.convert.WrapAsJava

class PrefboardNode(circuit:IntegratedCircuit) extends TNode
{
    var currentOp = CircuitOp.getOperation(0)

    /**
     * 0 - off
     * 1 - name only
     * 2 - minor details
     * 3 - all details
     */
    var detailLevel = 1
    var scale = 1.0
    var sizeMult = 8
    def size = circuit.size*sizeMult
    override def frame = Rect(position, Size((size.width*scale).toInt, (size.height*scale).toInt))

    private var leftMouseDown = false
    private var rightMouseDown = false
    private var mouseStart = Point(0, 0)

    private def isCircuitValid = circuit.size != Size.zeroSize

    private def toGridPoint(p:Point) =
    {
        val f = frame
        val rpos = p-position
        Point((rpos.x*circuit.size.width*1.0/f.width).toInt.min(circuit.size.width-1).max(0),
            (rpos.y*circuit.size.height*1.0/f.height).toInt.min(circuit.size.height-1).max(0))
    }

    private def toCenteredGuiPoint(gridP:Point) =
    {
        val dp = frame.size.vectorize/circuit.size.vectorize
        Point(gridP.vectorize*dp+dp/2)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        if (isCircuitValid)
        {
            val f = frame
            RenderCircuit.renderOrtho(circuit, f.x, f.y, size.width*scale, size.height*scale, rframe)

            if (rayTest(mouse) && !leftMouseDown)
                currentOp.renderHover(circuit, toGridPoint(mouse), f.x, f.y, size.width*scale, size.height*scale)
            else if (leftMouseDown)
                currentOp.renderDrag(circuit, mouseStart, toGridPoint(mouse), f.x, f.y, size.width*scale, size.height*scale)
        }
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (isCircuitValid && !leftMouseDown && rayTest(mouse))
        {
            val part = circuit.getPart(toGridPoint(mouse))
            if (part != null)
            {
                val data = part.getRolloverData(detailLevel)
                if (data.nonEmpty)
                {
                    ClipNode.tempDisableScissoring()
                    translateToScreen()
                    val Point(mx, my) = parent.convertPointToScreen(mouse)
                    GuiDraw.drawMultilineTip(mx+12, my-12, WrapAsJava.seqAsJavaList(data))
                    translateFromScreen()
                    ClipNode.tempEnableScissoring()
                }
            }
        }
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (isCircuitValid && !consumed && rayTest(p))
        {
            if (button == 0)
            {
                leftMouseDown = true
                mouseStart = toGridPoint(p)
            }
            else if (button == 1)
            {
                rightMouseDown = true

                val gridP = toGridPoint(p)
                circuit.getPart(gridP) match
                {
                    case gp:IGuiCircuitPart =>
                        val currentlyOpen = children.collect{case cg:CircuitGui => cg}
                        if (!currentlyOpen.exists(_.part == gp))
                        {
                            val gui = gp.createGui
                            gui.position = convertPointFrom(Point(4, 4)*(currentlyOpen.size+1), parent)
                            gui.linePointerCalc = () => toCenteredGuiPoint(gridP)
                            addChild(gui)
                            gui.pushZTo(currentlyOpen.size*0.1)
                        }

                    case _ =>
                }
            }
            true
        }
        else false
    }

    override def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (leftMouseDown)
        {
            leftMouseDown = false
            val mouseEnd = toGridPoint(p)
            val opUsed = circuit.sendOpUse(currentOp, mouseStart, mouseEnd)
            if (!opUsed && mouseEnd == mouseStart)
            {
                val part = circuit.getPart(mouseEnd)
                if (part != null) part.onClicked()
            }
        }
        if (rightMouseDown)
        {
            rightMouseDown = false
            val mouseEnd = toGridPoint(p)
            if (mouseEnd == mouseStart)
            {
                val part = circuit.getPart(mouseEnd)
                if (part != null) part.onActivated()
            }
        }
        false
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        if (!consumed && leftMouseDown && keycode == Keyboard.KEY_ESCAPE)
        {
            leftMouseDown = false
            true
        }
        else false
    }

    def incDetail(){detailLevel = math.min(detailLevel+1, 3)}
    def decDetail(){detailLevel = math.max(detailLevel-1, 0)}

    def incScale(){scale = math.min(scale+0.2, 3.0)}
    def decScale(){scale = math.max(scale-0.2, 0.5)}
}

class ICToolsetNode extends TNode
{
    var opSet = Seq[CircuitOp]()
    var buttonSize = Size(16, 16)
    var buttonGap = 1

    var opSelectDelegate = {_:CircuitOp => ()}

    private var focused = false
    private var toolButtons = Seq[NodeButton]()
    private var leadingButton:NodeButton = null
    private var nextLeadingButton:NodeButton = null

    def setup()
    {
        for (op <- opSet)
        {
            val b = createButtonFor(op)
            b.size = buttonSize
            addChild(b)
            toolButtons :+= b
        }

        for (op <- toolButtons.drop(1))
            op.hidden = true

        rotateButtons(toolButtons.head)
    }

    private def rotateButtons(nextLead:NodeButton)
    {
        if (nextLead == leadingButton) return
        leadingButton = nextLead
        leadingButton.position = Point.zeroPoint

        val delta = (opSet.size-1)*(buttonSize.width+buttonGap)
        val firstPoint = Point(-delta/2+buttonSize.width/2, -buttonSize.height-buttonGap)
        val subOps = toolButtons.filterNot(_ == leadingButton)
        for (i <- 0 until subOps.size)
        {
            val b = subOps(i)
            b.position = firstPoint.add(i*(buttonSize.width+buttonGap), 0)
        }
    }

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        if (nextLeadingButton != null)
            rotateButtons(nextLeadingButton)
    }

    private def buttonClicked(op:CircuitOp, button:NodeButton)
    {
        setFocused()
        opSelectDelegate(op)
        parent.children.collect
        {
            case t:ICToolsetNode if t != this => t
        }.foreach(_.setUnfocused())
        nextLeadingButton = button
        leadingButton.mouseoverLock = false
        button.mouseoverLock = true
    }

    def setUnfocused()
    {
        if (focused) hideSubTools()
        focused = false
        leadingButton.mouseoverLock = false
    }

    def setFocused()
    {
        if (!focused) unhideSubTools()
        focused = true
    }

    private def unhideSubTools()
    {
        for (b <- toolButtons)
            b.hidden = false
    }

    private def hideSubTools()
    {
        for (b <- toolButtons) if (b != leadingButton)
            b.hidden = true
    }

    private def createButtonFor(op:CircuitOp) =
    {
        val b = new NodeButtonIcon
        {
            override def drawButton(mouseover:Boolean)
            {
                op.renderImage(position.x+2, position.y+2, size.width-4, size.height-4)
            }
        }
        b.tooltipBuilder = {_ += op.getOpName}
        b.clickDelegate = {() => buttonClicked(op, b)}
        b
    }
}

class NewICNode extends TNode
{
    val size = Size(100, 120)
    override def frame = Rect(position, size)

    var sizerRenderSize = Size(50, 50)
    var sizerRenderOffset = Point(0, -16)
    var sizerRenderGap = 2

    var maxBoardSize = Size(4, 4)
    var selectedBoardSize = Size(1, 1)

    var completionDelegate = {() => ()}

    var outsideColour = Colors.LIGHT_GREY.argb
    var insideColour = Colors.CYAN.rgb|0x88000000
    var hoverColour = Colors.BLUE.argb

    def getName =
    {
        val t = textbox.text
        if (t.isEmpty) "untitled" else t
    }

    private var textbox:SimpleTextboxNode = null
    private var sizerMap:Map[(Int, Int), Rect] = null

    private def sizerPos = position+Point(size/2-sizerRenderSize/2)+sizerRenderOffset

    private def calcSizerRects =
    {
        val p = sizerPos
        val d = sizerRenderSize/maxBoardSize

        val rcol = GuiLib.createGrid(p.x, p.y, maxBoardSize.width, maxBoardSize.height, d.width, d.height)
        val icol = GuiLib.createGrid(0, 0, maxBoardSize.width, maxBoardSize.height, 1, 1)
        val zcol = rcol.zip(icol)

        var rects = Map[(Int, Int), Rect]()
        for (((px, py), (x, y)) <- zcol)
        {
            val rect = Rect(Point(px, py)+sizerRenderGap/2, d-sizerRenderGap/2)
            rects += (x, y) -> rect
        }
        rects
    }

    private def getMouseoverPos(mouse:Point) = sizerMap.find(_._2 contains mouse) match
    {
        case Some(((x, y), r)) => Point(x, y)
        case None => null
    }

    override def traceHit(absPoint:Point) = true

    override def onAddedToParent_Impl()
    {
        sizerMap = calcSizerRects

        val close = new NodeButtonMC
        close.size = Size(8, 8)
        close.position = Point(4, 4)
        close.clickDelegate = {() => removeFromParent()}
        addChild(close)

        val fin = new NodeButtonMC
        fin.size = Size(40, 15)
        fin.position = Point(size.width/2-fin.size.width/2, size.height-fin.size.height-4)
        fin.clickDelegate = {() =>
            removeFromParent()
            completionDelegate()
        }
        fin.text = "start"
        addChild(fin)

        textbox = new SimpleTextboxNode
        textbox.size = Size(80, 14)
        textbox.position = Point(size/2-textbox.size/2)+Point(0, 24)
        textbox.phantom = "untitled"
        addChild(textbox)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        GuiDraw.drawGradientRect(0, 0, parent.frame.width, parent.frame.height, -1072689136, -804253680)
        GuiLib.drawGuiBox(position.x, position.y, size.width, size.height, 0)

        val mousePos = getMouseoverPos(mouse)
        for (((x, y), rect) <- sizerMap)
        {
            GuiDraw.drawRect(rect.x, rect.y, rect.width, rect.height, outsideColour)

            if (x <= selectedBoardSize.width-1 && y <= selectedBoardSize.height-1)
                GuiDraw.drawRect(rect.x, rect.y, rect.width, rect.height, insideColour)

            if (mousePos != null && x == mousePos.x && y == mousePos.y)
                GuiDraw.drawRect(rect.midX-2, rect.midY-2, 4, 4, hoverColour)
        }
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (rayTest(mouse))
        {
            val mousePos = getMouseoverPos(mouse)
            if (mousePos != null)
            {
                translateToScreen()
                val Point(mx, my) = parent.convertPointToScreen(mouse)
                import scala.collection.JavaConversions._
                GuiDraw.drawMultilineTip(mx+12, my-12, Seq((mousePos.x+1)*16+" x "+(mousePos.y+1)*16))
                translateFromScreen()
            }
        }
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        if (!consumed)
        {
            val mousePos = getMouseoverPos(p)
            if (mousePos != null)
            {
                selectedBoardSize = Size(mousePos+1)
                return true
            }
        }
        false
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        if (!consumed && keycode == Keyboard.KEY_ESCAPE)
        {
            removeFromParent()
            true
        }
        else false
    }
}

class GuiICWorkbench(tile:TileICWorkbench) extends NodeGui(330, 256)
{
    var pref:PrefboardNode = null

    override def onAddedToParent_Impl()
    {
        val clip = new ClipNode
        clip.position = Point(7, 18)
        clip.size = Size(252, 197)
        addChild(clip)

        val pan = new NodePan
        pan.position = Point(0, 0)
        pan.size = Size(252, 197)
        pan.clampSize = Size(252, 197)-195
        pan.dragTestFunction = {() => Keyboard.isKeyDown(Keyboard.KEY_LSHIFT)}
        clip.addChild(pan)

        pref = new PrefboardNode(tile.circuit)
        pref.position = Point(pan.size/2-pref.size/2)
        pref.zPosition = -0.01//Must be below pan/clip nodes
        pan.addChild(pref)

        val toolbar = new TNode {}

        {
            import CircuitOpDefs._
            def addToolsetRange(from:OpDef, to:OpDef)
            {
                addToolset((from.getID to to.getID).map(CircuitOpDefs(_)))
            }
            def addToolset(opset:Seq[OpDef])
            {
                val toolset = new ICToolsetNode
                toolset.position = Point(17, 0)*toolbar.children.size
                toolset.opSet = opset.map(_.getOp)
                toolset.setup()
                toolset.opSelectDelegate = {op => pref.currentOp = op }
                toolbar.addChild(toolset)
            }

            addToolset(Seq(Erase))
            addToolset(Seq(Torch))
            addToolset(Seq(AlloyWire))
            addToolsetRange(WhiteInsulatedWire, BlackInsulatedWire)
            addToolsetRange(NeutralBundledCable, BlackBundledCable)
            addToolsetRange(SimpleIO, BundledIO)
            addToolset(Seq(ORGate))
        }

        addChild(toolbar)
        toolbar.position = Point(size.width/2-toolbar.calculateAccumulatedFrame.width/2, 235)

        val dminus = new NodeButtonMC
        dminus.position = Point(269, 175)
        dminus.size = Size(10, 10)
        dminus.text = "-"
        dminus.clickDelegate = {() => pref.decDetail()}
        addChild(dminus)

        val dplus = new NodeButtonMC
        dplus.position = Point(309, 175)
        dplus.size = Size(10, 10)
        dplus.text = "+"
        dplus.clickDelegate = {() => pref.incDetail()}
        addChild(dplus)

        val sminus = new NodeButtonMC
        sminus.position = Point(269, 207)
        sminus.size = Size(10, 10)
        sminus.text = "-"
        sminus.clickDelegate = {() => pref.decScale()}
        addChild(sminus)

        val splus = new NodeButtonMC
        splus.position = Point(309, 207)
        splus.size = Size(10, 10)
        splus.text = "+"
        splus.clickDelegate = {() => pref.incScale()}
        addChild(splus)

        val reqNew = new NodeButtonMC
        reqNew.position = Point(277, 133)
        reqNew.size = Size(34, 12)
        reqNew.text = "new"
        reqNew.clickDelegate = {() =>
            val nic = new NewICNode
            nic.position = Point(size/2)-Point(nic.size/2)
            nic.completionDelegate = {() =>
                val ic = new IntegratedCircuit
                ic.name = nic.getName
                ic.size = nic.selectedBoardSize*16
                val stream = tile.writeStream(TileICWorkbench.ICNEW)
                ic.writeDesc(stream)
                stream.sendToServer()
            }
            addChild(nic)
            nic.pushZTo(5)
        }
        addChild(reqNew)
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GL11.glColor4f(1, 1, 1, 1)
        PRResources.guiPrototyper.bind()
        Gui.func_146110_a(0, 0, 0, 0, size.width, size.height, 512, 512)

        GuiDraw.drawString("IC Workbench", 8, 6, Colors.GREY.argb, false)

        GuiDraw.drawStringC("detail", 273, 162, 42, 14, Colors.GREY.argb, false)
        GuiDraw.drawStringC(pref.detailLevel+"", 279, 175, 30, 10, Colors.GREY.argb, false)

        GuiDraw.drawStringC("scale", 273, 193, 42, 14, Colors.GREY.argb, false)
        GuiDraw.drawStringC(pref.scale+"", 279, 207, 30, 10, Colors.GREY.argb, false)
    }
}

object GuiICWorkbench extends TGuiBuilder
{
    override def getID = FabricationProxy.icWorkbenchGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        WorldLib.getTileEntity(Minecraft.getMinecraft.theWorld, data.readCoord()) match
        {
            case t:TileICWorkbench => new GuiICWorkbench(t)
            case _ => null
        }
    }
}