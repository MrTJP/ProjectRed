/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.Transformation
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.vec.Point

class CircuitPartTorch extends CircuitPart with TICAcquisitions with IPoweredCircuitPart
{
    override def getPartType = CircuitPartDefs.Torch

    override def onAdded()
    {
        if (!world.network.isRemote) notify(0xF)
    }

    override def onRemoved()
    {
        if (!world.network.isRemote) notify(0xF)
    }

    override def rsOutputLevel(r:Int) = 255
    override def canConnectRS(r:Int) = true

    @SideOnly(Side.CLIENT)
    override def getPartName = "Torch"

    @SideOnly(Side.CLIENT)
    override def renderDynamic(t:Transformation, ortho:Boolean, frame:Float) =
    {
        RenderCircuitTorch.render(t, ortho)
    }
}

class CircuitOpTorch extends CircuitOp
{
    override def checkOp(circuit:IntegratedCircuit, start:Point, end:Point) =
        circuit.getPart(end.x, end.y) == null

    override def writeOp(circuit:IntegratedCircuit, start:Point, end:Point, out:MCDataOutput)
    {
        out.writeByte(end.x).writeByte(end.y)
    }

    override def readOp(circuit:IntegratedCircuit, in:MCDataInput)
    {
        val point = Point(in.readUByte(), in.readUByte())
        if (circuit.getPart(point.x, point.y) == null)
            circuit.setPart(point.x, point.y, CircuitPartDefs.Torch.createPart)
    }


    @SideOnly(Side.CLIENT)
    override def getOpName = "Torch"
    override def renderImage(x:Double, y:Double, width:Double, height:Double){}
    override def renderHover(circuit:IntegratedCircuit, point:Point, x:Double, y:Double, xSize:Double, ySize:Double){}
    override def renderDrag(circuit:IntegratedCircuit, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double){}
}