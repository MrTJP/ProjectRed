/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.{Transformation, Translation}
import mrtjp.core.vec.Point
import mrtjp.projectred.fabrication.TileEditorOp._
import mrtjp.projectred.fabrication.ICComponentStore._
import net.minecraftforge.fml.relauncher.{Side, SideOnly}


abstract class OpWire extends TileEditorOp
{
    override def checkOp(editor:ICTileMapEditor, start:Point, end:Point) =
        editor.getPart(start.x, start.y) == null

    override def writeOp(editor:ICTileMapEditor, start:Point, end:Point, out:MCDataOutput)
    {
        out.writeByte(start.x).writeByte(start.y)
        out.writeByte(end.x).writeByte(end.y)
    }

    override def readOp(editor:ICTileMapEditor, in:MCDataInput)
    {
        val start = Point(in.readUByte(), in.readUByte())
        val end = Point(in.readUByte(), in.readUByte())
        val end2 = start+Point((end-start).vectorize.axialProject)

        for (px <- math.min(start.x, end2.x) to math.max(start.x, end2.x))
            for (py <- math.min(start.y, end2.y) to math.max(start.y, end2.y))
                if (!isOnBorder(editor.size, Point(px, py)))
                    if (editor.getPart(px, py) == null)
                        editor.setPart(px, py, createPart)
    }

    def createPart:ICTile

    @SideOnly(Side.CLIENT)
    override def renderHover(ccrs:CCRenderState, editor:ICTileMapEditor, point:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (editor.getPart(point) != null) return

        renderHolo(x, y, xSize, ySize, editor.size, point,
            if (isOnBorder(editor.size, point)) 0x33FF0000 else 0x33FFFFFF)

        val t = orthoPartT(x, y, xSize, ySize, editor.size, point.x, point.y)
        doRender(ccrs, t, 0)
    }

    @SideOnly(Side.CLIENT)
    override def renderDrag(ccrs:CCRenderState, editor:ICTileMapEditor, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (editor.getPart(start) != null) return

        val end2 = start+Point((end-start).vectorize.axialProject)

        for (px <- math.min(start.x, end2.x) to math.max(start.x, end2.x))
            for (py <- math.min(start.y, end2.y) to math.max(start.y, end2.y))
            {
                val point = Point(px, py)
                renderHolo(x, y, xSize, ySize, editor.size, point,
                    if (isOnBorder(editor.size, point)) 0x44FF0000 else 0x44FFFFFF)

                if (editor.getPart(px, py) == null)
                {
                    val t = orthoPartT(x, y, xSize, ySize, editor.size, px, py)
                    var m = 0
                    if (px > start.x) {m |= 8; if (px != end2.x) m |= 2}
                    if (px < start.x) {m |= 2; if (px != end2.x) m |= 8}
                    if (py > start.y) {m |= 1; if (py != end2.y) m |= 4}
                    if (py < start.y) {m |= 4; if (py != end2.y) m |= 1}
                    if (px == start.x && end2.x > start.x) m |= 2
                    if (px == start.x && end2.x < start.x) m |= 8
                    if (py == start.y && end2.y > start.y) m |= 4
                    if (py == start.y && end2.y < start.y) m |= 1
                    doRender(ccrs, t, m)
                }
            }
    }

    @SideOnly(Side.CLIENT)
    override def renderImage(ccrs:CCRenderState, x:Double, y:Double, width:Double, height:Double)
    {
        val t = orthoGridT(width, height) `with` new Translation(x, y, 0)
        doInvRender(ccrs, t)
    }

    @SideOnly(Side.CLIENT)
    def doRender(ccrs:CCRenderState, t:Transformation, conn:Int)
    @SideOnly(Side.CLIENT)
    def doInvRender(ccrs:CCRenderState, t:Transformation)
}

class OpAlloyWire extends OpWire
{
    override def createPart = ICTileDefs.AlloyWire.createPart

    @SideOnly(Side.CLIENT)
    override def doRender(ccrs:CCRenderState, t:Transformation, conn:Int)
    {
        val r = RenderTileAlloyWire
        r.connMap = conn.toByte
        r.signal = 255.toByte
        r.render(ccrs, t, true)
    }

    @SideOnly(Side.CLIENT)
    override def doInvRender(ccrs:CCRenderState, t:Transformation)
    {
        RenderTileAlloyWire.prepairInv()
        RenderTileAlloyWire.render(ccrs, t, true)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = createPart.getPartName
}

class OpInsulatedWire(colour:Int) extends OpWire
{
    override def createPart =
    {
        val part = ICTileDefs.InsulatedWire.createPart.asInstanceOf[InsulatedWireICTile]
        part.colour = colour.toByte
        part
    }

    @SideOnly(Side.CLIENT)
    override def doRender(ccrs:CCRenderState, t:Transformation, conn:Int)
    {
        val r = RenderTileInsulatedWire
        r.connMap = conn.toByte
        r.signal = 255.toByte
        r.colour = colour.toByte
        r.render(ccrs, t, true)
    }

    @SideOnly(Side.CLIENT)
    override def doInvRender(ccrs:CCRenderState, t:Transformation)
    {
        RenderTileInsulatedWire.prepairInv(colour)
        RenderTileInsulatedWire.render(ccrs, t, true)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = EnumColour.values()(colour&0xFF).name+" Insulated wire"
}

class OpBundledCable(colour:Int) extends OpWire
{
    override def createPart =
    {
        val part = ICTileDefs.BundledCable.createPart.asInstanceOf[BundledCableICTile]
        part.colour = colour.toByte
        part
    }

    @SideOnly(Side.CLIENT)
    override def doRender(ccrs:CCRenderState, t:Transformation, conn:Int)
    {
        val r = RenderTileBundledCable
        r.connMap = conn.toByte
        r.colour = colour.toByte
        r.render(ccrs, t, true)
    }

    @SideOnly(Side.CLIENT)
    override def doInvRender(ccrs:CCRenderState, t:Transformation)
    {
        RenderTileBundledCable.prepairInv(colour)
        RenderTileBundledCable.render(ccrs, t, true)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = (if (colour != -1) EnumColour.values()(colour&0xFF).name+" " else "")+"Bundled cable"
}