/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Vector3, Rotation, Transformation, Translation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.vec.{Point, Vec2}
import mrtjp.projectred.fabrication.CircuitOp._
import mrtjp.projectred.fabrication.ICComponentStore._

abstract class OpGateCommons(meta:Int) extends CircuitOp
{
    def canPlace(circuit:IntegratedCircuit, point:Point):Boolean
    def findRot(circuit:IntegratedCircuit, start:Point, end:Point):Int

    override def checkOp(circuit:IntegratedCircuit, start:Point, end:Point) =
        canPlace(circuit, start) && circuit.getPart(start) == null

    override def writeOp(circuit:IntegratedCircuit, start:Point, end:Point, out:MCDataOutput)
    {
        out.writeByte(start.x).writeByte(start.y)
        out.writeByte(findRot(circuit, start, end))
    }

    override def readOp(circuit:IntegratedCircuit, in:MCDataInput)
    {
        val point = Point(in.readByte(), in.readByte())
        val r = in.readUByte()

        if (circuit.getPart(point) == null && canPlace(circuit, point))
        {
            val part = CircuitPart.createPart(ICGateDefinition(meta).gateType).asInstanceOf[GateICPart]
            part.preparePlacement(r, meta)
            circuit.setPart(point, part)
        }
    }

    @SideOnly(Side.CLIENT)
    override def renderHover(circuit:IntegratedCircuit, point:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (circuit.getPart(point) != null) return

        val t = orthoPartT(x, y, xSize, ySize, circuit.size, point.x, point.y)
        doRender(t, findRot(circuit, point, point))

        renderHolo(x, y, xSize,  ySize, circuit.size, point,
            if (canPlace(circuit, point)) 0x33FFFFFF else 0x33FF0000)
    }

    @SideOnly(Side.CLIENT)
    override def renderDrag(circuit:IntegratedCircuit, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (circuit.getPart(start) != null) return

        val t = orthoPartT(x, y, xSize, ySize, circuit.size, start.x, start.y)
        doRender(t, findRot(circuit, start, end))

        renderHolo(x, y, xSize,  ySize, circuit.size, start,
            if (canPlace(circuit, start)) 0x44FFFFFF else 0x44FF0000)
    }

    @SideOnly(Side.CLIENT)
    override def renderImage(x:Double, y:Double, width:Double, height:Double)
    {
        val t = orthoGridT(width, height) `with` new Translation(x, y, 0)
        doRender(t, 0)
    }

    @SideOnly(Side.CLIENT)
    def doRender(t:Transformation, rot:Int)
    {
        RenderICGate.renderInv(Rotation.quarterRotations(rot).at(Vector3.center) `with` t, meta)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = ICGateDefinition(meta).unlocal
}

class OpGate(meta:Int) extends OpGateCommons(meta)
{
    override def findRot(circuit:IntegratedCircuit, start:Point, end:Point) =
    {
        (end-start).vectorize.axialProject.normalize match
        {
            case Vec2( 0,-1) => 0
            case Vec2( 1, 0) => 1
            case Vec2( 0, 1) => 2
            case Vec2(-1, 0) => 3
            case _ => 0
        }
    }

    override def canPlace(circuit:IntegratedCircuit, point:Point) =
        !isOnBorder(circuit.size, point)
}

class OpIOGate(meta:Int) extends OpGateCommons(meta)
{
    override def canPlace(circuit:IntegratedCircuit, point:Point) =
        isOnBorder(circuit.size, point) && !isOnEdge(circuit.size, point)

    override def findRot(circuit:IntegratedCircuit, start:Point, end:Point) =
    {
        val wm = circuit.size.width-1
        val hm = circuit.size.height-1
        start match
        {
            case Point(_, 0)    => 0
            case Point(`wm`, _) => 1
            case Point(_, `hm`) => 2
            case Point(0, _)    => 3
            case _              => 0
        }
    }
}