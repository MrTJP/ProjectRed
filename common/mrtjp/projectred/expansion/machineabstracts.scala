package mrtjp.projectred.expansion

import codechicken.core.{IGuiPacketSender, ServerUtils}
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.{BlockCoord, Vector3, Rotation, Transformation}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api.{IScrewdriver, IConnectable}
import mrtjp.projectred.core._
import mrtjp.projectred.core.blockutil.{BlockMulti, TileMulti}
import mrtjp.projectred.core.inventory.SpecialContainer
import mrtjp.projectred.core.utils.TPortableInventory
import mrtjp.projectred.transmission.{PowerWire_100v, FramedPowerWire_100v, WirePart}
import net.minecraft.block.material.Material
import net.minecraft.client.renderer.texture.IconRegister
import net.minecraft.entity.player.{EntityPlayerMP, EntityPlayer}
import net.minecraft.inventory.{ICrafting, Container, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.Icon
import net.minecraft.world.World
import net.minecraftforge.common.ForgeDirection

class BlockMachine(id:Int) extends BlockMulti(id, Material.rock)
{
    setHardness(2)
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def isOpaqueCube = true

    override def renderAsNormalBlock = true

    override def isBlockSolidOnSide(world:World, x:Int, y:Int, z:Int, side:ForgeDirection) = true

    override def registerIcons(reg:IconRegister)
    {
        super.registerIcons(reg)
        BlockMachine.iconIO = reg.registerIcon("projectred:machines/machineio")
        BlockMachine.bottom = reg.registerIcon("projectred:machines/machbottom")
        BlockMachine.top = reg.registerIcon("projectred:machines/machtop")
        BlockMachine.side = reg.registerIcon("projectred:machines/machside")

        BlockMachine.nowork = reg.registerIcon("projectred:machines/machnowork")
        BlockMachine.work = reg.registerIcon("projectred:machines/machwork")

        BlockMachine.furnaceFront = reg.registerIcon("projectred:machines/furnacefront")
        BlockMachine.furnaceFrontOn = reg.registerIcon("projectred:machines/furnacefronton")
    }

    override def getIcon(s:Int, md:Int) = s match
    {
        case 0 => BlockMachine.bottom
        case 1 => BlockMachine.top
        case _ => BlockMachine.side
    }
}

object BlockMachine
{
    var iconIO:Icon = _

    var bottom:Icon = _
    var top:Icon = _
    var side:Icon = _
    var nowork:Icon = _
    var work:Icon = _

    var furnaceFront:Icon = _
    var furnaceFrontOn:Icon = _
}

abstract class TileMachine extends TileMulti
{
    protected var orientation:Byte = 0

    def side = orientation>>2

    def setSide(s:Int)
    {
        orientation = (orientation&0x3|s<<2).asInstanceOf[Byte]
    }

    def rotation = orientation&0x3

    def setRotation(r:Int)
    {
        orientation = (orientation&0xFC|r).asInstanceOf[Byte]
    }

    def rotationT:Transformation = Rotation.sideOrientation(side, rotation).at(Vector3.center)

    override def onBlockPlaced(stack:ItemStack, s:Int, player:EntityPlayer)
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
        writeStreamSend(writeStream(1).writeByte(orientation))
    }

    def onBlockRotated() {}
}

trait TileMachineIO extends TileMachine with TPortableInventory with ISidedInventory
{
    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        inv.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        inv.load(tag)
    }

    abstract override def onInventoryChanged() = super.onInventoryChanged()

    abstract override def onBlockRemoval()
    {
        super.onBlockRemoval()
        inv.dropContents(worldObj, xCoord, yCoord, zCoord)
    }
}

trait TileGuiMachine extends TileMachine
{
    abstract override def onBlockActivated(player:EntityPlayer, side:Int) =
    {
        super.onBlockActivated(player, side) || openGui(player)
    }

    def openGui(player:EntityPlayer) =
    {
        if (!worldObj.isRemote && !player.isSneaking)
            ServerUtils.openSMPContainer(player.asInstanceOf[EntityPlayerMP], createContainer(player), new IGuiPacketSender
            {
                def sendPacket(player:EntityPlayerMP, windowId:Int)
                {
                    val p = new PacketCustom(ExpansionSPH.channel, ExpansionSPH.machine_gui_open)
                    p.writeCoord(xCoord, yCoord, zCoord)
                    p.writeByte(windowId).writeByte(guiID)
                    p.sendToPlayer(player)
                }
            })

        !player.isSneaking
    }

    def createContainer(player:EntityPlayer):Container

    def guiID:Int
}

trait TMachinePowerable extends TileMachine with TConnectableTileMulti with TPowerConnectable
{
    val cond:PowerConductor
    def condIds = (0 until 30) ++ (32 until 56)

    override def canConnect(part:IConnectable) =
        part.isInstanceOf[PowerWire_100v] ||
            part.isInstanceOf[FramedPowerWire_100v] ||
            (!part.isInstanceOf[WirePart] && part.isInstanceOf[TPowerConnectable])

    override def world = worldObj

    override def conductor(side:Int) = cond

    override def conductorOut(id:Int):PowerConductor =
    {
        if (0 until 24 contains id) //straight conns w/ edge rot
        {
            val absDir = id/4
            val edgeRot = id%4
            if ((connMap&(0x1<<edgeRot)<<absDir)!= 0)
            {
                val tp = BlockConnLib.getStraight(worldObj, absDir, edgeRot, new BlockCoord(this))

                if (tp.isInstanceOf[TPowerConnectable]) return tp.asInstanceOf[TPowerConnectable].conductor(absDir^1)
            }
        }
        else if (24 until 30 contains id) //straight face
        {
            val absDir = id-24
            if ((connMap&0x1000000<<absDir)!= 0)
            {
                val tp = BlockConnLib.getStraight(worldObj, absDir, -1, new BlockCoord(this))
                if (tp.isInstanceOf[TPowerConnectable]) return tp.asInstanceOf[TPowerConnectable].conductor(absDir^1)

                val pos = new BlockCoord(this).offset(absDir)
                val t = BasicUtils.getTileEntity(worldObj, pos, classOf[TPowerConnectable])
                if (t != null) return t.conductor(absDir^1)
            }
        }
        else if (32 until 56 contains id) //corner
        {
            val absDir = (id-32)/4
            val edgeRot = (id-32)/4
            if ((connMap&(0x100000000L<<edgeRot)<<absDir)!=0)
            {
                val sideTo = Rotation.rotateSide(absDir^1, edgeRot)
                val tp = BlockConnLib.getCorner(worldObj, absDir, edgeRot, new BlockCoord(this))
                if (tp != null && tp.isInstanceOf[TPowerConnectable]) return tp.asInstanceOf[TPowerConnectable].conductor(sideTo^1)
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
        writeStreamSend(ws)
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

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("werk", isWorking)
        tag.setInteger("rem", workRemaining)
        tag.setInteger("max", workMax)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        isWorking = tag.getBoolean("werk")
        workRemaining = tag.getInteger("rem")
        workMax = tag.getInteger("max")
    }

    override def onBlockNeighborChange(l:Int) = super.onBlockNeighborChange(l)

    override def onBlockRemoval() = super.onBlockRemoval()

    override def onInventoryChanged()
    {
        super.onInventoryChanged()
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
        writeStreamSend(writeStream(14).writeBoolean(isWorking))
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

class WorkingMachineContainer(player:EntityPlayer, tile:TileMachineWorking) extends SpecialContainer(player.inventory)
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