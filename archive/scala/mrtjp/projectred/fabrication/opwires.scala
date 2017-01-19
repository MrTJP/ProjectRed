/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Transformation, Translation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.vec.Point
import mrtjp.projectred.fabrication.CircuitOp._
import mrtjp.projectred.fabrication.ICComponentStore._


abstract class OpWire extends CircuitOp
{
    override def checkOp(circuit:IntegratedCircuit, start:Point, end:Point) =
        circuit.getPart(start.x, start.y) == null

    override def writeOp(circuit:IntegratedCircuit, start:Point, end:Point, out:MCDataOutput)
    {
        out.writeByte(start.x).writeByte(start.y)
        out.writeByte(end.x).writeByte(end.y)
    }

    override def readOp(circuit:IntegratedCircuit, in:MCDataInput)
    {
        val start = Point(in.readUByte(), in.readUByte())
        val end = Point(in.readUByte(), in.readUByte())
        val end2 = start+Point((end-start).vectorize.axialProject)

        for (px <- math.min(start.x, end2.x) to math.max(start.x, end2.x))
            for (py <- math.min(start.y, end2.y) to math.max(start.y, end2.y))
                if (!isOnBorder(circuit.size, Point(px, py)))
                    if (circuit.getPart(px, py) == null)
                        circuit.setPart(px, py, createPart)
    }

    def createPart:CircuitPart

    @SideOnly(Side.CLIENT)
    override def renderHover(circuit:IntegratedCircuit, point:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (circuit.getPart(point) != null) return

        renderHolo(x, y, xSize, ySize, circuit.size, point,
            if (isOnBorder(circuit.size, point)) 0x33FF0000 else 0x33FFFFFF)

        val t = orthoPartT(x, y, xSize, ySize, circuit.size, point.x, point.y)
        doRender(t, 0)
    }

    @SideOnly(Side.CLIENT)
    override def renderDrag(circuit:IntegratedCircuit, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (circuit.getPart(start) != null) return

        val end2 = start+Point((end-start).vectorize.axialProject)

        for (px <- math.min(start.x, end2.x) to math.max(start.x, end2.x))
            for (py <- math.min(start.y, end2.y) to math.max(start.y, end2.y))
            {
                val point = Point(px, py)
                renderHolo(x, y, xSize, ySize, circuit.size, point,
                    if (isOnBorder(circuit.size, point)) 0x44FF0000 else 0x44FFFFFF)

                if (circuit.getPart(px, py) == null)
                {
                    val t = orthoPartT(x, y, xSize, ySize, circuit.size, px, py)
                    var m = 0
                    if (px > start.x) {m |= 8; if (px != end2.x) m |= 2}
                    if (px < start.x) {m |= 2; if (px != end2.x) m |= 8}
                    if (py > start.y) {m |= 1; if (py != end2.y) m |= 4}
                    if (py < start.y) {m |= 4; if (py != end2.y) m |= 1}
                    if (px == start.x && end2.x > start.x) m |= 2
                    if (px == start.x && end2.x < start.x) m |= 8
                    if (py == start.y && end2.y > start.y) m |= 4
                    if (py == start.y && end2.y < start.y) m |= 1
                    doRender(t, m)
                }
            }
    }

    @SideOnly(Side.CLIENT)
    override def renderImage(x:Double, y:Double, width:Double, height:Double)
    {
        val t = orthoGridT(width, height) `with` new Translation(x, y, 0)
        doInvRender(t)
    }

    @SideOnly(Side.CLIENT)
    def doRender(t:Transformation, conn:Int)
    @SideOnly(Side.CLIENT)
    def doInvRender(t:Transformation)
}

class OpAlloyWire extends OpWire
{
    override def createPart = CircuitPartDefs.AlloyWire.createPart

    @SideOnly(Side.CLIENT)
    override def doRender(t:Transformation, conn:Int)
    {
        val r = RenderICAlloyWire
        r.connMap = conn.toByte
        r.signal = 255.toByte
        r.render(t, true)
    }

    @SideOnly(Side.CLIENT)
    override def doInvRender(t:Transformation)
    {
        RenderICAlloyWire.prepairInv()
        RenderICAlloyWire.render(t, true)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = createPart.getPartName
}

class OpInsulatedWire(colour:Int) extends OpWire
{
    override def createPart =
    {
        val part = CircuitPartDefs.InsulatedWire.createPart.asInstanceOf[InsulatedWireICPart]
        part.colour = colour.toByte
        part
    }

    @SideOnly(Side.CLIENT)
    override def doRender(t:Transformation, conn:Int)
    {
        val r = RenderICInsulatedWire
        r.connMap = conn.toByte
        r.signal = 255.toByte
        r.colour = colour.toByte
        r.render(t, true)
    }

    @SideOnly(Side.CLIENT)
    override def doInvRender(t:Transformation)
    {
        RenderICInsulatedWire.prepairInv(colour)
        RenderICInsulatedWire.render(t, true)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = Colors(colour&0xFF).name+" Insulated wire"
}

class OpBundledCable(colour:Int) extends OpWire
{
    override def createPart =
    {
        val part = CircuitPartDefs.BundledCable.createPart.asInstanceOf[BundledCableICPart]
        part.colour = colour.toByte
        part
    }

    @SideOnly(Side.CLIENT)
    override def doRender(t:Transformation, conn:Int)
    {
        val r = RenderICBundledCable
        r.connMap = conn.toByte
        r.colour = colour.toByte
        r.render(t, true)
    }

    @SideOnly(Side.CLIENT)
    override def doInvRender(t:Transformation)
    {
        RenderICBundledCable.prepairInv(colour)
        RenderICBundledCable.render(t, true)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = (if (colour != -1) Colors(colour&0xFF).name+" " else "")+"Bundled cable"
}