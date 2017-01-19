/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.vec.Transformation
import cpw.mods.fml.relauncher.{Side, SideOnly}

class TorchICPart extends CircuitPart with TICAcquisitions with IPoweredCircuitPart
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
    override def getPickOp = CircuitOpDefs.Torch.getOp

    @SideOnly(Side.CLIENT)
    override def renderDynamic(t:Transformation, ortho:Boolean, frame:Float) =
    {
        RenderICTorch.render(t, ortho)
    }
}

class CircuitOpTorch extends SimplePlacementOp
{
    override def doPartRender(t:Transformation) = RenderICTorch.render(t, true)

    override def createPart = CircuitPartDefs.Torch.createPart

    @SideOnly(Side.CLIENT)
    override def getOpName = "Torch"
}