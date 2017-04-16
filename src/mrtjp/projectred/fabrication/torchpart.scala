///*
// * Copyright (c) 2015.
// * Created by MrTJP.
// * All rights reserved.
// */
//package mrtjp.projectred.fabrication
//
//import codechicken.lib.render.CCRenderState
//import codechicken.lib.vec.Transformation
//import net.minecraftforge.fml.relauncher.{Side, SideOnly}
//
//class TorchICPart extends CircuitPart with TICAcquisitions with IRedwireICGate
//{
//    override def getPartType = CircuitPartDefs.Torch
//
//    override def onAdded()
//    {
//        if (!world.network.isRemote) notify(0xF)
//    }
//
//    override def onRemoved()
//    {
//        if (!world.network.isRemote) notify(0xF)
//    }
//
//    override def rsOutputLevel(r:Int) = 255
//    override def canConnectRS(r:Int) = true
//
//    @SideOnly(Side.CLIENT)
//    override def getPartName = "Torch"
//
//    @SideOnly(Side.CLIENT)
//    override def getPickOp = CircuitOpDefs.Torch.getOp
//
//    @SideOnly(Side.CLIENT)
//    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float) =
//    {
//        RenderICTorch.render(ccrs, t, ortho)
//    }
//}
//
//class CircuitOpTorch extends SimplePlacementOp
//{
//    override def doPartRender(ccrs:CCRenderState, t:Transformation) = RenderICTorch.render(ccrs, t, true)
//
//    override def createPart = CircuitPartDefs.Torch.createPart
//
//    @SideOnly(Side.CLIENT)
//    override def getOpName = "Torch"
//}