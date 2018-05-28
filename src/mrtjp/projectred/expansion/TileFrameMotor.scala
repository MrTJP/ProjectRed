/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.model.bakery.SimpleBlockRenderer
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import codechicken.multipart.{IRedstoneConnector, RedstoneInteractions}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api._
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.common.capabilities.Capability
import net.minecraftforge.common.property.IExtendedBlockState

trait TMotorTile extends TileMachine with TPoweredMachine with IFrame with IRedstoneConnector with IMovementCallback
{
    var isCharged = false
    var isPowered = false
    var isMoving = false

    private var moveDesc:IMovementDescriptor = null

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("ch", isCharged)
        tag.setBoolean("pow", isPowered)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        isCharged = tag.getBoolean("ch")
        isPowered = tag.getBoolean("pow")
        oldC = isCharged
    }

    abstract override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
    }

    abstract override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        isCharged = in.readBoolean()
    }

    abstract override def read(in:MCDataInput, key:Int) =  key match
    {
        case 2 =>
            isCharged = in.readBoolean()
            isMoving = in.readBoolean()
            markRender()
        case _ => super.read(in, key)
    }

    def sendStateUpdate()
    {
        sendWriteStream(writeStream(2).writeBoolean(isCharged).writeBoolean(isMoving))
    }

    override def setDescriptor(desc:IMovementDescriptor){ moveDesc = desc }

    override def onMovementStarted()
    {
        isMoving = true
        sendStateUpdate()
    }

    override def onMovementFinished()
    {
        isMoving = false
        sendStateUpdate()
    }

    abstract override def onNeighborBlockChange()
    {
        val oldPow = isPowered

        isPowered = false
        import scala.util.control.Breaks._
        breakable { for (s <- 0 until 6) {
            if (RedstoneInteractions.getPowerTo(getWorld, getPos, s, 0x1F) > 0) {
                isPowered = true
                break()
            }
        }}

        if (!oldPow && isPowered && !isMoving && cond.canWork) {
            val moveBlockPos = getPos.offset(EnumFacing.values()(side^1))
            if (!getWorld.isAirBlock(moveBlockPos)) {
                if (!ProjectRedAPI.relocationAPI.isMoving(getWorld, moveBlockPos) &&
                        !ProjectRedAPI.relocationAPI.isMoving(getWorld, getPos))
                {
                    val blocks = ProjectRedAPI.relocationAPI.getStickResolver
                            .getStructure(getWorld, moveBlockPos, getPos)

                    val r = ProjectRedAPI.relocationAPI.getRelocator
                    r.push()
                    r.setWorld(getWorld)
                    r.setDirection(getMoveDir)
                    r.setSpeed(1/16D)
                    r.setCallback(this)
                    r.addBlocks(blocks)
                    r.execute()
                    r.pop()
                }
            }
        }
    }

    def getMoveDir:Int

    def drawPower(size:Int)

    abstract override def updateServer()
    {
        super.updateServer()
        if (isMoving) drawPower(moveDesc.getSize)
        if (getWorld.getTotalWorldTime%10 == 0) updateRendersIfNeeded()
    }

    private var oldC = false
    def updateRendersIfNeeded()
    {
        isCharged = cond.canWork
        if (oldC != isCharged)
            sendStateUpdate()

        oldC = isCharged
    }

    override def getConnectionMask(side:Int) = if ((side^1) == this.side) 0 else 0x1F
    override def weakPowerLevel(side:Int, mask:Int) = 0

    override def hasCapability(capability: Capability[_], facing: EnumFacing): Boolean = {
        if (capability == ProjectRedAPI.relocationAPI.getFrameCapability) return true
        super.hasCapability(capability, facing)
    }

    override def getCapability[T](capability: Capability[T], facing: EnumFacing): T = {
        if (capability == ProjectRedAPI.relocationAPI.getFrameCapability) return this.asInstanceOf[T]
        super.getCapability(capability, facing)
    }
}

class TileFrameMotor extends TileMachine with TMotorTile
{
    override def getBlock = ProjectRedExpansion.machine2
    override def doesRotate = true
    override def doesOrient = true

    override def stickOut(w:World, pos:BlockPos, side:EnumFacing) = side.ordinal == (this.side^1)
    override def stickIn(w:World, pos:BlockPos, side:EnumFacing) = side.ordinal != (this.side^1)

    override def getMoveDir = absoluteDir((rotation+2)%4)

    override def drawPower(size:Int) = cond.drawPower(100+10*size)
}

object RenderFrameMotor extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import mrtjp.projectred.expansion.BlockProperties._
    import org.apache.commons.lang3.tuple.Triple

    var bottom:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    var side2a:TextureAtlasSprite = _
    var side2b:TextureAtlasSprite = _
    var side2c:TextureAtlasSprite = _
    var side4:TextureAtlasSprite = _
    var side5:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _
    var iconT3:UVTransformation = _


    override def handleState(state:IExtendedBlockState, world:IBlockAccess, pos:BlockPos) = world.getTileEntity(pos) match {
        case t:TileFrameMotor =>
            var s = state
            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[Integer])
            s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_WORKING_PROPERTY, t.isMoving.asInstanceOf[JBool])
            s = s.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
            s
        case _ => state
    }

    override def getWorldTransforms(state:IExtendedBlockState) = {
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
        val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
        val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
        Triple.of(side, rotation,
            if (isWorking && isCharged) iconT3
            else if (isCharged) iconT2
            else iconT1)
    }

    override def getItemTransforms(stack:ItemStack) = Triple.of(0, 0, iconT1)

    override def shouldCull() = true

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/motor/bottom"))
        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/motor/top"))
        side2a = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/motor/side2a"))
        side2b = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/motor/side2b"))
        side2c = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/motor/side2c"))
        side4 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/motor/side4"))
        side5 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/motor/side5"))

        iconT1 = new MultiIconTransformation(bottom, top, side2a, side2a, side4, side5)
        iconT2 = new MultiIconTransformation(bottom, top, side2b, side2b, side4, side5)
        iconT3 = new MultiIconTransformation(bottom, top, side2c, side2c, side4, side5)
    }
}
