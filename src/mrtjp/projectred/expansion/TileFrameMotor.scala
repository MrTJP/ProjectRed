///*
// * Copyright (c) 2015.
// * Created by MrTJP.
// * All rights reserved.
// */
//package mrtjp.projectred.expansion
//
//import codechicken.lib.data.{MCDataInput, MCDataOutput}
//import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
//import codechicken.multipart.{IRedstoneConnector, RedstoneInteractions}
//import mrtjp.core.render.TCubeMapRender
//import mrtjp.core.world.WorldLib
//import mrtjp.mcframes.api.{IFrame, MCFramesAPI}
//import mrtjp.projectred.ProjectRedExpansion
//import mrtjp.relocation.api.{BlockPos, IMovementCallback, IMovementDescriptor, RelocationAPI}
//import net.minecraft.block.Block
//import net.minecraft.client.renderer.texture.IIconRegister
//import net.minecraft.nbt.NBTTagCompound
//import net.minecraft.util.IIcon
//import net.minecraft.world.{IBlockAccess, World}
//
//trait TMotorTile extends TileMachine with TPoweredMachine with IFrame with IRedstoneConnector with IMovementCallback
//{
//    var isCharged = false
//    var isPowered = false
//    var isMoving = false
//
//    private var moveDesc:IMovementDescriptor = null
//
//    abstract override def save(tag:NBTTagCompound)
//    {
//        super.save(tag)
//        tag.setBoolean("ch", isCharged)
//        tag.setBoolean("pow", isPowered)
//    }
//
//    abstract override def load(tag:NBTTagCompound)
//    {
//        super.load(tag)
//        isCharged = tag.getBoolean("ch")
//        isPowered = tag.getBoolean("pow")
//        oldC = isCharged
//    }
//
//    abstract override def writeDesc(out:MCDataOutput)
//    {
//        super.writeDesc(out)
//        out.writeBoolean(isCharged)
//    }
//
//    abstract override def readDesc(in:MCDataInput)
//    {
//        super.readDesc(in)
//        isCharged = in.readBoolean()
//    }
//
//    abstract override def read(in:MCDataInput, key:Int) =  key match
//    {
//        case 2 =>
//            isCharged = in.readBoolean()
//            isMoving = in.readBoolean()
//            markRender()
//        case _ => super.read(in, key)
//    }
//
//    def sendStateUpdate()
//    {
//        writeStream(2).writeBoolean(isCharged).writeBoolean(isMoving).sendToChunk()
//    }
//
//    override def setDescriptor(desc:IMovementDescriptor){ moveDesc = desc }
//
//    override def onMovementStarted()
//    {
//        isMoving = true
//        sendStateUpdate()
//    }
//
//    override def onMovementFinished()
//    {
//        isMoving = false
//        sendStateUpdate()
//    }
//
//    abstract override def onNeighborChange(b:Block)
//    {
//        super.onNeighborChange(b)
//
//        val oldPow = isPowered
//
//        isPowered = false
//        import scala.util.control.Breaks._
//        breakable { for (s <- 0 until 6) {
//            if (RedstoneInteractions.getPowerTo(world, x, y, z, s, 0x1F) > 0) {
//                isPowered = true
//                break()
//            }
//        }}
//
//        if (!oldPow && isPowered && !isMoving && cond.canWork)
//        {
//            val pos = position.offset(side^1)
//            if (!world.isAirBlock(pos.x, pos.y, pos.z))
//            {
//                if (!RelocationAPI.instance.isMoving(world, pos.x, pos.y, pos.z) &&
//                        !RelocationAPI.instance.isMoving(world, x, y, z))
//                {
//                    val blocks = MCFramesAPI.instance.getStickResolver
//                            .getStructure(world, pos.x, pos.y, pos.z, new BlockPos(x, y, z))
//
//                    val r = RelocationAPI.instance.getRelocator
//                    r.push()
//                    r.setWorld(world)
//                    r.setDirection(getMoveDir)
//                    r.setSpeed(1/16D)
//                    r.setCallback(this)
//                    r.addBlocks(blocks)
//                    r.execute()
//                    r.pop()
//                }
//            }
//        }
//    }
//
//    def getMoveDir:Int
//
//    def drawPower(size:Int)
//
//    abstract override def update()
//    {
//        super.update()
//        if (isMoving) drawPower(moveDesc.getSize)
//        if (world.getTotalWorldTime%10 == 0) updateRendersIfNeeded()
//    }
//
//    private var oldC = false
//    def updateRendersIfNeeded()
//    {
//        isCharged = cond.canWork
//        if (oldC != isCharged)
//            sendStateUpdate()
//
//        oldC = isCharged
//    }
//
//    override def getConnectionMask(side:Int) = if ((side^1) == this.side) 0 else 0x1F
//    override def weakPowerLevel(side:Int, mask:Int) = 0
//}
//
//class TileFrameMotor extends TileMachine with TMotorTile
//{
//    override def getBlock = ProjectRedExpansion.machine2
//    override def doesRotate = true
//    override def doesOrient = true
//
//    override def stickOut(w:World, x:Int, y:Int, z:Int, side:Int) = side == (this.side^1)
//    override def stickIn(w:World, x:Int, y:Int, z:Int, side:Int) = side != (this.side^1)
//
//    override def getMoveDir = absoluteDir((rotation+2)%4)
//
//    override def drawPower(size:Int) = cond.drawPower(100+10*size)
//}
//
//object RenderFrameMotor extends TCubeMapRender
//{
//    var bottom:IIcon = _
//    var top:IIcon = _
//    var side2a:IIcon = _
//    var side2b:IIcon = _
//    var side2c:IIcon = _
//    var side4:IIcon = _
//    var side5:IIcon = _
//
//    var iconT1:UVTransformation = null
//    var iconT2:UVTransformation = null
//    var iconT3:UVTransformation = null
//
//    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
//    {
//        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileFrameMotor])
//        if (te != null) (te.side, te.rotation,
//                if (te.isCharged && te.isMoving) iconT3
//                else if (te.isCharged) iconT2
//                else iconT1)
//        else getInvData
//    }
//
//    override def getInvData = (0, 0, iconT1)
//
//    override def getIcon(s:Int, meta:Int) = s match
//    {
//        case 0 => bottom
//        case 1 => top
//        case 2 => side2a
//        case 3 => side2a
//        case 4 => side4
//        case 5 => side5
//    }
//
//    override def registerIcons(reg:IIconRegister)
//    {
//        bottom = reg.registerIcon("projectred:mechanical/motor/bottom")
//        top = reg.registerIcon("projectred:mechanical/motor/top")
//        side2a = reg.registerIcon("projectred:mechanical/motor/side2a")
//        side2b = reg.registerIcon("projectred:mechanical/motor/side2b")
//        side2c = reg.registerIcon("projectred:mechanical/motor/side2c")
//        side4 = reg.registerIcon("projectred:mechanical/motor/side4")
//        side5 = reg.registerIcon("projectred:mechanical/motor/side5")
//
//        iconT1 = new MultiIconTransformation(bottom, top, side2a, side2a, side4, side5)
//        iconT2 = new MultiIconTransformation(bottom, top, side2b, side2b, side4, side5)
//        iconT3 = new MultiIconTransformation(bottom, top, side2c, side2c, side4, side5)
//    }
//}
