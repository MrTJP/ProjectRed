/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import java.math.MathContext

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.pipeline.ColourMultiplier
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{UVScale, UVTranslation}
import com.mojang.realmsclient.gui.ChatFormatting
import mrtjp.core.gui._
import mrtjp.core.vec.{Point, Rect, Size}
import mrtjp.projectred.fabrication.ICComponentStore._
import net.minecraft.client.gui.Gui
import net.minecraft.client.renderer.GlStateManager._
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.input.{Keyboard, Mouse}

import scala.collection.JavaConversions._
import scala.collection.convert.WrapAsJava
import scala.collection.immutable.ListMap

class PrefboardNode(circuit:ICTileMapEditor) extends TNode
{
    var currentOp:CircuitOp = _

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

    var opPickDelegate = {_:CircuitOp => ()}

    private var leftMouseDown = false
    private var rightMouseDown = false
    private var mouseStart = Point(0, 0)

    private def isCircuitValid = circuit.nonEmpty

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

    override def update_Impl()
    {
        if (mcInst.theWorld.getTotalWorldTime%20 == 0)
            circuit.refreshErrors()
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        if (isCircuitValid) {
            val f = frame
            val ccrs = CCRenderState.instance()

            color(1, 1, 1, 1)

            RenderCircuit.renderOrtho(ccrs, circuit, f.x, f.y, size.width*scale, size.height*scale, rframe)

            if (currentOp != null)
            {
                if (frame.contains(mouse) && rayTest(mouse) && !leftMouseDown)
                    currentOp.renderHover(ccrs, circuit, toGridPoint(mouse), f.x, f.y, size.width*scale, size.height*scale)
                else if (leftMouseDown)
                    currentOp.renderDrag(ccrs, circuit, mouseStart, toGridPoint(mouse), f.x, f.y, size.width*scale, size.height*scale)
            }

            if (mcInst.theWorld.getTotalWorldTime%100 > 5 && circuit.errors.nonEmpty)
            {
                prepairRender(ccrs)
                TextureUtils.changeTexture(GuiICWorkbench.background)

                for ((Point(x, y), (_, c)) <- circuit.errors)
                {
                    val t = orthoPartT(f.x, f.y, size.width*scale, size.height*scale, circuit.size, x, y)
                    faceModels(dynamicIdx(0, true)).render(ccrs,
                        t, new UVScale(64) `with` new UVTranslation(330, 37) `with` new UVScale(1/512D),
                        ColourMultiplier.instance(EnumColour.values()(c).rgba)
                    )
                }
                finishRender(ccrs)
            }
        }
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (isCircuitValid && !leftMouseDown && frame.contains(mouse) && rayTest(mouse))
        {
            val point = toGridPoint(mouse)
            val part = circuit.getPart(point)
            if (part != null)
            {
                val data = part.getRolloverData(detailLevel)
                if (data.nonEmpty)
                {
                    ClipNode.tempDisableScissoring()
                    translateToScreen()
                    val Point(mx, my) = parent.convertPointToScreen(mouse)
                    GuiDraw.drawMultilineTip(mx+12, my-12, WrapAsJava.seqAsJavaList(data))
                    if (circuit.errors.contains(point))
                        GuiDraw.drawMultilineTip(mx+12, my-32, Seq(ChatFormatting.RED.toString+circuit.errors(point)._1))
                    translateFromScreen()
                    ClipNode.tempEnableScissoring()
                }
            }
        }
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        if (isCircuitValid && !consumed && rayTest(p)) button match
        {
            case 0 =>
                leftMouseDown = true
                mouseStart = toGridPoint(p)
                return true
            case 1 =>
                rightMouseDown = true
                val gridP = toGridPoint(p)
                circuit.getPart(gridP) match
                {
                    case gp:IGuiICTile =>
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
                return true
            case _ if button == mcInst.gameSettings.keyBindPickBlock.getKeyCode =>
                doPickOp()
                return true
            case _ =>
        }
        false
    }

    override def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (leftMouseDown)
        {
            leftMouseDown = false
            val mouseEnd = toGridPoint(p)
            val opUsed = currentOp != null && circuit.sendOpUse(currentOp, mouseStart, mouseEnd)
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

    override def mouseScrolled_Impl(p:Point, dir:Int, consumed:Boolean) =
    {
        if (!consumed && rayTest(p))
        {
            if (dir > 0) rescaleAt(p, math.min(scale+0.1, 3.0))
            else if (dir < 0) rescaleAt(p, math.max(scale-0.1, 0.5))
            true
        }
        else false
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        import Keyboard._
        if (!consumed) keycode match
        {
            case KEY_ESCAPE if leftMouseDown =>
                leftMouseDown = false
                true
            case KEY_ESCAPE if currentOp != null =>
                opPickDelegate(null)
                true
            case _ if keycode == mcInst.gameSettings.keyBindPickBlock.getKeyCode =>
                doPickOp()
                true
            case _ if keycode == mcInst.gameSettings.keyBindInventory.getKeyCode =>
                opPickDelegate(CircuitOpDefs.Erase.getOp)
                true
            case _ => false
        }
        else false
    }

    def doPickOp()
    {
        val root = getRoot
        val i = Mouse.getX*root.width/root.mc.displayWidth
        val j = root.height-Mouse.getY*root.height/root.mc.displayHeight-1
        val absPos = Point(i, j)

        val pos = parent.convertPointFromScreen(absPos)
        if (rayTest(pos))
        {
            val part = circuit.getPart(toGridPoint(pos))
            opPickDelegate(if (part != null) part.getPickOp else null)
        }
    }

    def incDetail(){detailLevel = math.min(detailLevel+1, 3)}
    def decDetail(){detailLevel = math.max(detailLevel-1, 0)}

    def incScale(){rescaleAt(frame.midPoint, math.min(scale+0.2, 3.0))}
    def decScale(){rescaleAt(frame.midPoint, math.max(scale-0.2, 0.5))}

    def rescaleAt(point:Point, newScale:Double)
    {
        val p = parent.convertPointTo(point, this).vectorize
        val newP = (p/scale)*newScale
        val dp = newP-p
        scale = newScale
        position -= Point(dp)
    }
}

class ICToolsetNode extends TNode
{
    var opSet = Seq.empty[CircuitOp]
    var title = ""
    var buttonSize = Size(16, 16)
    var buttonGap = 1

    var opSelectDelegate = {_:CircuitOp => ()}

    private var focused = false
    private var buttonOpMap = ListMap.empty[ButtonNode, CircuitOp]

    private var leadingButton:ButtonNode = null
    private var groupButton:ButtonNode = null

    def setup()
    {
        for (op <- opSet)
        {
            val b = createButtonFor(op)
            b.size = buttonSize
            b.hidden = true
            addChild(b)
            buttonOpMap += b -> op
        }

        val delta = opSet.size*(buttonSize.width+buttonGap)
        val firstPoint = Point(-delta/2+buttonSize.width/2, -buttonSize.height-buttonGap)
        for ((b, i) <- buttonOpMap.keys.zipWithIndex)
            b.position = firstPoint.add(i*(buttonSize.width+buttonGap), 0)

        leadingButton = buttonOpMap.head._1

        groupButton = new IconButtonNode {
            override def drawButton(mouseover:Boolean) =
            {
                val op = buttonOpMap(leadingButton)
                op.renderImage(CCRenderState.instance(), position.x+2, position.y+2, size.width-4, size.height-4)
            }
        }
        groupButton.size = buttonSize
        groupButton.tooltipBuilder = {_ += buttonOpMap(leadingButton).getOpName}
        groupButton.clickDelegate = {() => leadingButton.clickDelegate() }
        addChild(groupButton)
    }

    private def buttonClicked(op:CircuitOp, button:ButtonNode)
    {
        setFocused()
        opSelectDelegate(op)
        parent.children.collect {
            case t:ICToolsetNode if t != this => t
        }.foreach(_.setUnfocused())
        leadingButton.mouseoverLock = false
        leadingButton = button
        leadingButton.mouseoverLock = true
    }

    def setUnfocused()
    {
        if (focused) hideSubTools()
        focused = false
        groupButton.mouseoverLock = false
    }

    def setFocused()
    {
        if (!focused) unhideSubTools()
        focused = true
        groupButton.mouseoverLock = true
    }

    private def unhideSubTools()
    {
        if (buttonOpMap.size > 1) for (b <- buttonOpMap.keys)
            b.hidden = false
    }

    private def hideSubTools()
    {
        if (buttonOpMap.size > 1) for (b <- buttonOpMap.keys)
            b.hidden = true
    }

    private def createButtonFor(op:CircuitOp) =
    {
        val b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                op.renderImage(CCRenderState.instance(), position.x+2, position.y+2, size.width-4, size.height-4)
            }
        }
        b.tooltipBuilder = {_ += op.getOpName}
        b.clickDelegate = {() => buttonClicked(op, b)}
        b
    }

    def pickOp(op:CircuitOp)
    {
        setUnfocused()
        buttonOpMap.find(_._2 == op) match
        {
            case Some((b, _)) => b.clickDelegate()
            case _ =>
        }
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (title.nonEmpty && groupButton.rayTest(parent.convertPointTo(mouse, this)))
        {
            import ChatFormatting._
            translateToScreen()
            val Point(mx, my) = parent.convertPointToScreen(mouse)
            GuiDraw.drawMultilineTip(mx+12, my-32, Seq(AQUA.toString+ITALIC.toString+title))
            translateFromScreen()
        }
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

    var outsideColour = EnumColour.LIGHT_GRAY.argb
    var insideColour = EnumColour.CYAN.argb(0x88)
    var hoverColour = EnumColour.BLUE.argb

    def getName =
    {
        val t = textbox.text
        if (t.isEmpty) "untitled" else t
    }

    private var textbox:SimpleTextboxNode = _
    private var sizerMap:Map[(Int, Int), Rect] = _

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

        val close = new MCButtonNode
        close.size = Size(8, 8)
        close.position = Point(4, 4)
        close.clickDelegate = {() => removeFromParent()}
        addChild(close)

        val fin = new MCButtonNode
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

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        if (!parent.asInstanceOf[GuiICWorkbench].tile.hasBP)
            removeFromParent()
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
        if (!consumed) keycode match
        {
            case Keyboard.KEY_ESCAPE =>
                removeFromParent()
                true
            case Keyboard.KEY_RETURN =>
                removeFromParent()
                completionDelegate()
                true
            case _ => false
        }
        else false
    }
}

class InfoNode extends TNode
{
    val size = Size(18, 18)
    override def frame = Rect(position, size)

    private def getTile = parent.asInstanceOf[GuiICWorkbench].tile

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        TextureUtils.changeTexture(GuiICWorkbench.background)

        if (!getTile.hasBP || getTile.getIC.isEmpty)
            Gui.drawModalRectWithCustomSizedTexture(position.x, position.y, 330, 0, size.width, size.height, 512, 512)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        val text =
            if (!getTile.hasBP)
                "Lay down a blueprint on the workbench."
            else if (getTile.getIC.isEmpty)
                "Blueprint is empty. Redraw it."
            else ""
        if (text.nonEmpty && rayTest(mouse))
        {
            translateToScreen()
            val Point(mx, my) = parent.convertPointToScreen(mouse)
            import scala.collection.JavaConversions._
            GuiDraw.drawMultilineTip(mx+12, my-12, Seq(text))
            translateFromScreen()
        }
    }
}

class GuiICWorkbench(val tile:TileICWorkbench) extends NodeGui(330, 256)
{
    var pref:PrefboardNode = null
    var toolSets = Seq[ICToolsetNode]()

    override def onAddedToParent_Impl()
    {
        val clip = new ClipNode
        clip.position = Point(7, 18)
        clip.size = Size(252, 197)
        addChild(clip)

        val pan = new PanNode
        pan.size = Size(252, 197)
        pan.clampSlack = 35
        pan.dragTestFunction = {() => Keyboard.isKeyDown(Keyboard.KEY_LSHIFT)}
        clip.addChild(pan)

        pref = new PrefboardNode(tile.circuit)
        pref.position = Point(pan.size/2-pref.size/2)
        pref.zPosition = -0.01//Must be below pan/clip nodes
        pref.opPickDelegate = {op =>
            if (op == null) pref.currentOp = null
            toolSets.foreach(_.pickOp(op))
        }
        pan.addChild(pref)

        val toolbar = new TNode {}

        {
            import CircuitOpDefs._
            def addToolsetRange(name:String, from:OpDef, to:OpDef)
            {
                addToolset(name, (from.getID to to.getID).map(CircuitOpDefs(_)))
            }
            def addToolset(name:String, opset:Seq[OpDef])
            {
                val toolset = new ICToolsetNode
                toolset.position = Point(17, 0)*toolbar.children.size
                toolset.title = name
                toolset.opSet = opset.map(_.getOp)
                toolset.setup()
                toolset.opSelectDelegate = {op => pref.currentOp = op }
                toolbar.addChild(toolset)
                toolSets :+= toolset
            }

            addToolset("", Seq(Erase))
            addToolset("Debug", Seq(/*Torch,*/ Lever, Button))
            addToolset("", Seq(AlloyWire))
            addToolsetRange("Insulated wires", WhiteInsulatedWire, BlackInsulatedWire)
            addToolsetRange("Bundled cables", NeutralBundledCable, BlackBundledCable)
            addToolset("IOs", Seq(SimpleIO, BundledIO, AnalogIO))
            addToolset("Primatives", Seq(ORGate, NORGate, NOTGate, ANDGate))//, NANDGate, XORGate, XNORGate, BufferGate, MultiplexerGate))
//            addToolset("Timing and Clocks", Seq(PulseFormerGate, RepeaterGate, TimerGate, SequencerGate, StateCellGate))
//            addToolset("Latches", Seq(SRLatchGate, ToggleLatchGate, TransparentLatchGate))
//            addToolset("Cells", Seq(NullCellGate, InvertCellGate, BufferCellGate))
//            addToolset("Misc", Seq(RandomizerGate, CounterGate, SynchronizerGate, DecRandomizerGate))
        }

        addChild(toolbar)
        toolbar.position = Point(size.width/2-toolbar.calculateAccumulatedFrame.width/2, 235)

        val dminus = new MCButtonNode
        dminus.position = Point(269, 175)
        dminus.size = Size(10, 10)
        dminus.text = "-"
        dminus.clickDelegate = {() => pref.decDetail()}
        addChild(dminus)

        val dplus = new MCButtonNode
        dplus.position = Point(309, 175)
        dplus.size = Size(10, 10)
        dplus.text = "+"
        dplus.clickDelegate = {() => pref.incDetail()}
        addChild(dplus)

        val sminus = new MCButtonNode
        sminus.position = Point(269, 207)
        sminus.size = Size(10, 10)
        sminus.text = "-"
        sminus.clickDelegate = {() => pref.decScale()}
        addChild(sminus)

        val splus = new MCButtonNode
        splus.position = Point(309, 207)
        splus.size = Size(10, 10)
        splus.text = "+"
        splus.clickDelegate = {() => pref.incScale()}
        addChild(splus)

        val reqNew = new MCButtonNode
        reqNew.position = Point(272, 133)
        reqNew.size = Size(44, 12)
        reqNew.text = "redraw"
        reqNew.clickDelegate = {() =>
            if (tile.hasBP) {
                val nic = new NewICNode
                nic.position = Point(size/2)-Point(nic.size/2)
                nic.completionDelegate = {() =>
                    val ic = new ICTileMapEditor
                    ic.tileMapContainer.name = nic.getName
                    ic.tileMapContainer.size = nic.selectedBoardSize*16
                    tile.sendNewICToServer(ic)
                }
                addChild(nic)
                nic.pushZTo(5)
            }
        }
        addChild(reqNew)

        val info = new InfoNode
        info.position = Point(241, 18)
        info.zPosition = 1
        addChild(info)
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        color(1, 1, 1, 1)

        TextureUtils.changeTexture(GuiICWorkbench.background)
        Gui.drawModalRectWithCustomSizedTexture(0, 0, 0, 0, size.width, size.height, 512, 512)

        GuiDraw.drawString("IC Workbench", 8, 6, EnumColour.GRAY.argb, false)

        GuiDraw.drawStringC("detail", 273, 162, 42, 14, EnumColour.GRAY.argb, false)
        GuiDraw.drawStringC(pref.detailLevel+"", 279, 175, 30, 10, EnumColour.GRAY.argb, false)

        GuiDraw.drawStringC("scale", 273, 193, 42, 14, EnumColour.GRAY.argb, false)
        GuiDraw.drawStringC(BigDecimal(pref.scale, new MathContext(2))+"", 279, 207, 30, 10, EnumColour.GRAY.argb, false)
    }
}

object GuiICWorkbench extends TGuiFactory
{
    val background = new ResourceLocation("projectred", "textures/gui/ic_workbench.png")

    override def getID = FabricationProxy.icWorkbenchGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        player.worldObj.getTileEntity(data.readPos()) match {
            case t:TileICWorkbench =>
                t.circuit.readDesc(data)
                new GuiICWorkbench(t)
            case _ => null
        }
    }
}