package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Rotation, Vector3}
import mrtjp.core.block.{InstancedBlock, InstancedBlockTile, TTileOrient}
import mrtjp.core.gui.WidgetContainer
import mrtjp.core.inventory.TInventory
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api._
import mrtjp.projectred.core._
import net.minecraft.block.material.Material
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{Container, ICrafting, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.util.ForgeDirection

class BlockMachine(name:String) extends InstancedBlock(name, Material.rock)
{
    setHardness(2)
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def isOpaqueCube = true

    override def renderAsNormalBlock = true

    override def isSideSolid(w:IBlockAccess, x:Int, y:Int, z:Int, side:ForgeDirection) = true
}

abstract class TileMachine extends InstancedBlockTile with TTileOrient
{
    override def onBlockPlaced(s:Int, meta:Int, player:EntityPlayer, stack:ItemStack, hit:Vector3)
    {
        setSide(if (doesOrient) s^1 else 0)
        setRotation(Rotation.getSidedRotation(player, side^1))
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(orientation)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        orientation = in.readByte
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setByte("rot", orientation)
    }

    override def load(tag:NBTTagCompound)
    {
        orientation = tag.getByte("rot")
    }

    override def read(in:MCDataInput, switchkey:Int) = switchkey match
    {
        case 1 =>
            orientation = in.readByte()
            markRender()
        case _ => super.read(in, switchkey)
    }

    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
    {
        val held = player.getHeldItem
        if (held != null && held.getItem.isInstanceOf[IScrewdriver])
        {
            if (worldObj.isRemote) return true
            def rotate() = if (doesRotate)
            {
                val old = rotation
                do setRotation((rotation+1)%4) while (old != rotation && !isRotationAllowed(rotation))
                if (old != rotation) sendOrientUpdate()
                onBlockRotated()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(worldObj, player)
            }
            def orient() = if (doesOrient)
            {
                val old = side
                do setSide((side+1)%6) while (old != side && !isSideAllowed(side))
                if (old != side) sendOrientUpdate()
                onBlockRotated()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(worldObj, player)
            }

            if (player.isSneaking || !doesOrient) rotate() else orient()

            return true
        }
        false
    }

    def isRotationAllowed(rot:Int) = true

    def isSideAllowed(s:Int) = true

    def doesRotate = true

    def doesOrient = false

    def sendOrientUpdate()
    {
        writeStream(1).writeByte(orientation).sendToChunk()
    }

    def onBlockRotated() {}
}

trait TileMachineIO extends TileMachine with TInventory with ISidedInventory
{
    abstract override def saveInv(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
    }

    abstract override def loadInv(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
    }

    abstract override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(getWorldObj, xCoord, yCoord, zCoord)
    }
}

trait TileGuiMachine extends TileMachine
{
    abstract override def onBlockActivated(player:EntityPlayer, side:Int) =
    {
        if (super.onBlockActivated(player, side)) true
        else if (!world.isRemote && !player.isSneaking)
        {
            openGui(player)
            true
        }
        else false
    }

    def openGui(player:EntityPlayer)

    def createContainer(player:EntityPlayer):Container
}

trait TMachinePowerable extends TileMachine with TConnectableInstTile with IPowerConnectable
{
    val cond:PowerConductor
    def condIds = (0 until 30) ++ (32 until 56)

    override def canConnect(part:IConnectable) = part match
    {
        case w:IPowerConnectable => true
        case _ => false
    }

    override def world = getWorldObj

    override def conductor(side:Int) = cond

    override def conductorOut(id:Int):PowerConductor =
    {
        if (0 until 24 contains id) //straight conns w/ edge rot
        {
            val s = id/4
            val edgeRot = id%4
            if (maskConnectsStraight(s, edgeRot)) getStraight(s, edgeRot) match
            {
                case tp:IPowerConnectable => return tp.conductor(s^1)
                case _ =>
            }
        }
        else if (24 until 30 contains id) //straight face
        {
            val s = id-24
            if (maskConnectsStraightCenter(s)) getStraightCenter(s) match
            {
                case tp:IPowerConnectable => return tp.conductor(s^1)
                case _ => WorldLib.getTileEntity(world, posOfInternal.offset(s), classOf[IPowerConnectable]) match
                {
                    case tp:IPowerConnectable => return tp.conductor(s^1)
                    case _ =>
                }
            }
        }
        else if (32 until 56 contains id) //corner
        {
            val s = (id-32)/4
            val edgeRot = (id-32)/4
            if (maskConnectsCorner(s, edgeRot)) getCorner(s, edgeRot) match
            {
                case tp:IPowerConnectable => return tp.conductor(Rotation.rotateSide(s^1, edgeRot)^1)
                case _ =>
            }
        }
        null
    }

    abstract override def update()
    {
        super.update()
        cond.update()
    }

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        cond.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        cond.load(tag)
    }
}

trait TileMachineSideConfig extends TileMachine
{
    var sideConfig = new Array[Int](6)
    var maxConfig = createSideConfig

    def createSideConfig:Array[Int]
    def sideInfo:Array[String]

    def toByteArray =
    {
        val b = new Array[Byte](6)
        for (i <- 0 until 6)
            b(i) = sideConfig(i).asInstanceOf[Byte]
        b
    }

    def sideUp(side:Int) = if (side != Rotation.rotateSide(this.side, rotation))
    {
        writeStream(15).writeByte(side).sendToServer()
    }

    def sideDown(side:Int) = if (side != Rotation.rotateSide(this.side, rotation))
    {
        writeStream(16).writeByte(side).sendToServer()
    }

    def resetAll()
    {
        writeStream(17).sendToServer()
    }

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setIntArray("sides", sideConfig)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        sideConfig = tag.getIntArray("sides")
    }

    def sendSideChange()
    {
        val ws = writeStream(18).writeByteArray(toByteArray)
        ws.sendToChunk()
    }

    override def read(in:MCDataInput, switchkey:Int) = switchkey match
    {
        case 15 => serverSideUp(in.readUByte())
        case 16 => serverSideDown(in.readUByte())
        case 17 => serverSideReset()
        case 18 =>
            val b = in.readByteArray(6)
            for (i <- 0 until 6) sideConfig(i) = b(i)
            markRender()
        case _  => super.read(in, switchkey)
    }

    private def serverSideUp(side:Int)
    {
        sideConfig(side) = sideConfig(side)+1
        if (sideConfig(side)>maxConfig(side)) sideConfig(side) = 0
        sendSideChange()
    }

    private def serverSideDown(side:Int)
    {
        sideConfig(side) = sideConfig(side)-1
        if (sideConfig(side)<0) sideConfig(side) = maxConfig(side)
        sendSideChange()
    }

    private def serverSideReset()
    {
        sideConfig = new Array[Int](6)
        sendSideChange()
    }

    abstract override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByteArray(toByteArray)
    }

    abstract override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        val b = in.readByteArray(6)
        for (i <- 0 until 6) sideConfig(i) = b(i)
    }

    abstract override def onBlockRotated()
    {
        super.onBlockRotated()
        serverSideReset()
    }
}

abstract class TileMachineWorking extends TileMachine
with TMachinePowerable with TileMachineIO with TileGuiMachine with TileMachineSideConfig
{
    val cond = new PowerConductor(this, condIds) with TPowerFlow

    var isWorking = false
    var unpergedWork = false

    var workRemaining = 0
    var workMax = 0

    def canStart = false
    def canFinish = workRemaining <= 0 && canStart

    def startWork() {}
    def endWork() {}
    def transfer() {}

    def calcDoableWork = if (cond.canWork) 1 else 0
    def drainPower(work:Int) = cond.drawPower(work*1000.0D)

    def enabled = true

    override def update()
    {
        super.update()
        val old = isWorking

        if (isWorking)
        {
            if (workRemaining>0)
            {
                val pow = calcDoableWork
                drainPower(pow)
                workRemaining -= pow
            }
            if (canFinish)
            {
                endWork()
                transfer()
                drainPower(workRemaining)
                workMax = 0

                if (canStart && enabled) startWork()
                else
                {
                    isWorking = false
                    unpergedWork = true
                    if (!isTickScheduled) scheduleTick(100)
                }
            }
        }
        else if (enabled)
        {
            if (transferTime) transfer()
            val pow = calcDoableWork
            if (startWorkTime && pow>0 && canStart)
            {
                startWork()
                drainPower(pow)
                workRemaining -= pow
                isWorking = true
            }
        }

        if (old != isWorking && isWorking)
        {
            sendWorkUpdate()
            unpergedWork = false
        }
    }

    override def onScheduledTick()
    {
        super.onScheduledTick()
        if (unpergedWork) sendWorkUpdate()
        unpergedWork = false
    }

    def transferTime = worldObj.getTotalWorldTime%32 == 0
    def startWorkTime = worldObj.getTotalWorldTime%4 == 0

    def progressScaled(scale:Int):Int =
    {
        if (!isWorking || workMax <= 0 || workRemaining <= 0) return 0
        scale*(workMax-workRemaining)/workMax
    }

    override def saveInv(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("werk", isWorking)
        tag.setInteger("rem", workRemaining)
        tag.setInteger("max", workMax)
    }

    override def loadInv(tag:NBTTagCompound)
    {
        super.load(tag)
        isWorking = tag.getBoolean("werk")
        workRemaining = tag.getInteger("rem")
        workMax = tag.getInteger("max")
    }

    override def markDirty()
    {
        super.markDirty()
        if (isWorking && !canStart)
        {
            isWorking = false
            unpergedWork = true
            workRemaining = 0
            workMax = 0
            if (!isTickScheduled) scheduleTick(100)
        }
    }

    def sendWorkUpdate()
    {
        writeStream(14).writeBoolean(isWorking).sendToChunk()
    }

    override def read(in:MCDataInput, switchkey:Int) = switchkey match
    {
        case 14 =>
            isWorking = in.readBoolean()
            markRender()
            markLight()
        case _ => super.read(in, switchkey)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(isWorking)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        isWorking = in.readBoolean()
    }

    override def getLightValue = if (isWorking) 13 else 0
}

class WorkingMachineContainer(player:EntityPlayer, tile:TileMachineWorking) extends WidgetContainer
{
    var ch = 0
    var work = 0
    var workMax = 0
    var fl = 0

    def sendPower = true
    def sendWork = true

    override def detectAndSendChanges()
    {
        super.detectAndSendChanges()
        import scala.collection.JavaConversions._
        for (i <- crafters)
        {
            val ic = i.asInstanceOf[ICrafting]

            if (sendPower && ch != tile.cond.charge) ic.sendProgressBarUpdate(this, 0, tile.cond.charge)
            if (sendPower && fl != tile.cond.flow)
            {
                ic.sendProgressBarUpdate(this, 1, tile.cond.flow&0xFFFF)
                ic.sendProgressBarUpdate(this, 2, tile.cond.flow>>16&0xFFFF)
            }

            if (sendWork && work != tile.workRemaining) ic.sendProgressBarUpdate(this, 3, tile.workRemaining)
            if (sendWork && workMax != tile.workMax) ic.sendProgressBarUpdate(this, 4, tile.workMax)
        }
        ch = tile.cond.charge
        fl = tile.cond.flow
        work = tile.workRemaining
        workMax = tile.workMax
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 0 => tile.cond.charge = bar
        case 1 => tile.cond.flow = tile.cond.flow&0xFFFF0000|bar&0xFFFF
        case 2 => tile.cond.flow = tile.cond.flow&0xFFFF|(bar&0xFFFF)<<16
        case 3 => tile.workRemaining = bar
        case 4 => tile.workMax = bar
    }
}