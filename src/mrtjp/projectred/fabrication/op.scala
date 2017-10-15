/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.{Transformation, Translation}
import mrtjp.core.util.Enum
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.fabrication.TileEditorOp._
import mrtjp.projectred.fabrication.ICComponentStore._
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

object TileEditorOpDefs extends Enum
{
    type EnumVal = OpDef

    //tools
    val Erase = OpDef(new TileEditorOpErase)

    //primitives
    val Lever = OpDef(new OpLever)
    val Button = OpDef(new OpButton)

    //alloy wire
    val AlloyWire = OpDef(new OpAlloyWire)

    //insulated wires
    val WhiteInsulatedWire = OpDef(new OpInsulatedWire(0))
    val OrangeInsulatedWire = OpDef(new OpInsulatedWire(1))
    val MagentaInsulatedWire = OpDef(new OpInsulatedWire(2))
    val LightBlueInsulatedWire = OpDef(new OpInsulatedWire(3))
    val YellowInsulatedWire = OpDef(new OpInsulatedWire(4))
    val LimeInsulatedWire = OpDef(new OpInsulatedWire(5))
    val PinkInsulatedWire = OpDef(new OpInsulatedWire(6))
    val GreyInsulatedWire = OpDef(new OpInsulatedWire(7))
    val LightGreyInsulatedWire = OpDef(new OpInsulatedWire(8))
    val CyanInsulatedWire = OpDef(new OpInsulatedWire(9))
    val PurpleInsulatedWire = OpDef(new OpInsulatedWire(10))
    val BlueInsulatedWire = OpDef(new OpInsulatedWire(11))
    val BrownInsulatedWire = OpDef(new OpInsulatedWire(12))
    val GreenInsulatedWire = OpDef(new OpInsulatedWire(13))
    val RedInsulatedWire = OpDef(new OpInsulatedWire(14))
    val BlackInsulatedWire = OpDef(new OpInsulatedWire(15))

    //bundled cables
    val NeutralBundledCable = OpDef(new OpBundledCable(-1))
    val WhiteBundledCable = OpDef(new OpBundledCable(0))
    val OrangeBundledCable = OpDef(new OpBundledCable(1))
    val MagentaBundledCable = OpDef(new OpBundledCable(2))
    val LightBlueBundledCable = OpDef(new OpBundledCable(3))
    val YellowBundledCable = OpDef(new OpBundledCable(4))
    val LimeBundledCable = OpDef(new OpBundledCable(5))
    val PinkBundledCable = OpDef(new OpBundledCable(6))
    val GreyBundledCable = OpDef(new OpBundledCable(7))
    val LightGreyBundledCable = OpDef(new OpBundledCable(8))
    val CyanBundledCable = OpDef(new OpBundledCable(9))
    val PurpleBundledCable = OpDef(new OpBundledCable(10))
    val BlueBundledCable = OpDef(new OpBundledCable(11))
    val BrownBundledCable = OpDef(new OpBundledCable(12))
    val GreenBundledCable = OpDef(new OpBundledCable(13))
    val RedBundledCable = OpDef(new OpBundledCable(14))
    val BlackBundledCable = OpDef(new OpBundledCable(15))

    //ios
    val SimpleIO = OpDef(new OpIOGate(ICGateDefinition.IOSimple.ordinal))
    val AnalogIO = OpDef(new OpIOGate(ICGateDefinition.IOAnalog.ordinal))
    val BundledIO = OpDef(new OpIOGate(ICGateDefinition.IOBundled.ordinal))

    //gates
    val ORGate = OpDef(new OpGate(ICGateDefinition.OR.ordinal))
    val NORGate = OpDef(new OpGate(ICGateDefinition.NOR.ordinal))
    val NOTGate = OpDef(new OpGate(ICGateDefinition.NOT.ordinal))
    val ANDGate = OpDef(new OpGate(ICGateDefinition.AND.ordinal))
    val NANDGate = OpDef(new OpGate(ICGateDefinition.NAND.ordinal))
    val XORGate = OpDef(new OpGate(ICGateDefinition.XOR.ordinal))
    val XNORGate = OpDef(new OpGate(ICGateDefinition.XNOR.ordinal))
    val BufferGate = OpDef(new OpGate(ICGateDefinition.Buffer.ordinal))
    val MultiplexerGate = OpDef(new OpGate(ICGateDefinition.Multiplexer.ordinal))
    val PulseFormerGate = OpDef(new OpGate(ICGateDefinition.Pulse.ordinal))
    val RepeaterGate = OpDef(new OpGate(ICGateDefinition.Repeater.ordinal))
    val RandomizerGate = OpDef(new OpGate(ICGateDefinition.Randomizer.ordinal))
    val SRLatchGate = OpDef(new OpGate(ICGateDefinition.SRLatch.ordinal))
    val ToggleLatchGate = OpDef(new OpGate(ICGateDefinition.ToggleLatch.ordinal))
    val TransparentLatchGate = OpDef(new OpGate(ICGateDefinition.TransparentLatch.ordinal))
    val TimerGate = OpDef(new OpGate(ICGateDefinition.Timer.ordinal))
    val SequencerGate = OpDef(new OpGate(ICGateDefinition.Sequencer.ordinal))
    val CounterGate = OpDef(new OpGate(ICGateDefinition.Counter.ordinal))
    val StateCellGate = OpDef(new OpGate(ICGateDefinition.StateCell.ordinal))
    val SynchronizerGate = OpDef(new OpGate(ICGateDefinition.Synchronizer.ordinal))
//    val DecRandomizerGate = OpDef(new OpGate(ICGateDefinition.DecRandomizer.ordinal))
//    val NullCellGate = OpDef(new OpGate(ICGateDefinition.NullCell.ordinal))
//    val InvertCellGate = OpDef(new OpGate(ICGateDefinition.InvertCell.ordinal))
//    val BufferCellGate = OpDef(new OpGate(ICGateDefinition.BufferCell.ordinal))

    val INSULATED = WhiteInsulatedWire to BlackInsulatedWire toArray
    val BUNDLED = NeutralBundledCable to BlackBundledCable toArray

    case class OpDef(op:TileEditorOp) extends Value
    {
        op.id = ordinal

        override def name = "op["+ordinal+"]"

        def getID = ordinal
        def getOp = op
    }
}

object TileEditorOp
{
    def getOperation(id:Int) = TileEditorOpDefs(id).getOp

    def renderHolo(x:Double, y:Double, xSize:Double, ySize:Double, csize:Size, point:Point, colour:Int)
    {
        val x1 = (x+xSize/csize.width*point.x).toInt
        val y1 = (y+ySize/csize.height*point.y).toInt
        val x2 = (x+xSize/csize.width*(point.x+1)).toInt
        val y2 = (y+ySize/csize.height*(point.y+1)).toInt

        GuiDraw.drawRect(x1, y1, x2-x1, y2-y1, colour)
    }

    def isOnBorder(cSize:Size, point:Point) =
        point.x == 0 || point.y == 0 || point.x == cSize.width-1 || point.y == cSize.height-1

    def isOnCorner(cSize:Size, point:Point) =
        point == Point(0, 0) || point == Point(0, cSize.height-1) || point == Point(cSize.width-1, 0) || point == Point(cSize.width-1, cSize.height-1)
}

trait TileEditorOp
{
    var id = -1

    def checkOp(editor:ICTileMapEditor, start:Point, end:Point):Boolean

    def writeOp(editor:ICTileMapEditor, start:Point, end:Point, out:MCDataOutput)
    def readOp(editor:ICTileMapEditor, in:MCDataInput)

    @SideOnly(Side.CLIENT)
    def getOpName:String
    @SideOnly(Side.CLIENT)
    def renderHover(ccrs:CCRenderState, editor:ICTileMapEditor, point:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    @SideOnly(Side.CLIENT)
    def renderDrag(ccrs:CCRenderState, editor:ICTileMapEditor, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    @SideOnly(Side.CLIENT)
    def renderImage(ccrs:CCRenderState, x:Double, y:Double, width:Double, height:Double)
}

abstract class SimplePlacementOp extends TileEditorOp
{
    def canPlace(editor:ICTileMapEditor, point:Point):Boolean =
        !isOnBorder(editor.size, point)

    override def checkOp(editor:ICTileMapEditor, start:Point, end:Point) =
        canPlace(editor, end) && editor.getTile(end) == null

    override def writeOp(editor:ICTileMapEditor, start:Point, end:Point, out:MCDataOutput)
    {
        out.writeByte(end.x).writeByte(end.y)
    }

    override def readOp(editor: ICTileMapEditor, in: MCDataInput)
    {
        val point = Point(in.readUByte(), in.readUByte())
        if (canPlace(editor, point) && editor.getTile(point) == null)
            editor.setTile(point, createPart)
    }

    @SideOnly(Side.CLIENT)
    override def renderImage(ccrs:CCRenderState, x:Double, y:Double, width:Double, height:Double)
    {
        val t = orthoGridT(width, height) `with` new Translation(x, y, 0)
        doPartRender(ccrs, t)
    }

    @SideOnly(Side.CLIENT)
    override def renderHover(ccrs:CCRenderState, editor:ICTileMapEditor, point:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (editor.getTile(point) != null) return

        renderHolo(x, y, xSize,  ySize, editor.size, point,
            if (!isOnBorder(editor.size, point)) 0x33FFFFFF else 0x33FF0000)

        val t = orthoPartT(x, y, xSize, ySize, editor.size, point.x, point.y)
        doPartRender(ccrs, t)
    }

    @SideOnly(Side.CLIENT)
    override def renderDrag(ccrs:CCRenderState, editor:ICTileMapEditor, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (editor.getTile(end) != null) return

        renderHolo(x, y, xSize,  ySize, editor.size, end,
            if (!isOnBorder(editor.size, end)) 0x44FFFFFF else 0x44FF0000)

        val t = orthoPartT(x, y, xSize, ySize, editor.size, end.x, end.y)
        doPartRender(ccrs, t)
    }

    def doPartRender(ccrs:CCRenderState, t:Transformation)

    def createPart:ICTile
}