package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Rotation, Vector3}
import mrtjp.core.block.{InstancedBlock, InstancedBlockTile, TTileOrient}
import mrtjp.core.gui.NodeContainer
import mrtjp.core.inventory.TInventory
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api._
import mrtjp.projectred.core._
import mrtjp.projectred.expansion.TileMachine._
import net.minecraft.block.material.Material
import net.minecraft.entity.player.{EntityPlayer, EntityPlayerMP}
import net.minecraft.inventory.{Container, ICrafting, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.util.ForgeDirection

import scala.ref.WeakReference

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
    def fakePlayer = fake.get.orNull

    def reloadFakePlayer() {
        if (fakePlayer == null)
            fake = FakePlayerUtils.getFakePlayerPR(world)
    }

    override def onBlockPlaced(s:Int, meta:Int, player:EntityPlayer, stack:ItemStack, hit:Vector3)
    {
        setSide(if (doesOrient) calcFacing(player) else 0)
        setRotation(if (doesRotate) (Rotation.getSidedRotation(player, side^1)+2)%4 else 0)
    }

    def calcFacing(ent:EntityPlayer):Int =
    {
        val yawrx = Math.floor(ent.rotationYaw*4.0F/360.0F+0.5D).toInt&0x3
        if ((Math.abs(ent.posX-x) < 2.0D) && (Math.abs(ent.posZ-z) < 2.0D))
        {
            val p = ent.posY+1.82D-ent.yOffset-y
            if (p > 2.0D) return 0
            if (p < 0.0D) return 1
        }
        yawrx match
        {
            case 0 => 3
            case 1 => 4
            case 2 => 2
            case _ => 5
        }
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

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 =>
            orientation = in.readByte()
            markRender()
        case _ => super.read(in, key)
    }

    override def onBlockActivated(player:EntityPlayer, actside:Int):Boolean =
    {
        val held = player.getHeldItem
        if ((doesRotate || doesOrient) && held != null && held.getItem.isInstanceOf[IScrewdriver]
                && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held))
        {
            if (world.isRemote) return true
            def rotate()
            {
                val old = rotation
                do setRotation((rotation+1)%4) while (old != rotation && !isRotationAllowed(rotation))
                if (old != rotation) sendOrientUpdate()
                world.notifyBlocksOfNeighborChange(x, y, z, getBlock)
                onBlockRotated()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }
            def orient()
            {
                val old = side
                do setSide((side+1)%6) while (old != side && !isSideAllowed(side))
                if (old != side) sendOrientUpdate()
                world.notifyBlocksOfNeighborChange(x, y, z, getBlock)
                onBlockRotated()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
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

    def onBlockRotated(){}
}

object TileMachine {
    var fake = WeakReference[EntityPlayerMP](null)
}

trait TGuiMachine extends TileMachine
{
    abstract override def onBlockActivated(player:EntityPlayer, side:Int) =
    {
        if (super.onBlockActivated(player, side)) true
        else if (!player.isSneaking)
        {
            if (!world.isRemote) openGui(player)
            true
        }
        else false
    }

    def openGui(player:EntityPlayer)

    def createContainer(player:EntityPlayer):Container
}

trait TPoweredMachine extends TileMachine with TPowerTile with ILowLoadMachine
{
    val cond = new PowerConductor(this, idRange) with TPowerDrawPoint

    override def conductor(dir:Int) = cond

    override def canConnectPart(part:IConnectable, s:Int, edgeRot:Int) = part match
    {
        case t:ILowLoadPowerLine => true
        case t:ILowLoadMachine => true
        case _ => false
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

abstract class TileProcessingMachine extends TileMachine
with TPoweredMachine with TGuiMachine with TInventory with ISidedInventory
{
    var isCharged = false
    var isWorking = false
    var workRemaining = 0
    var workMax = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("ch", isCharged)
        tag.setBoolean("work", isWorking)
        tag.setInteger("rem", workRemaining)
        tag.setInteger("max", workMax)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        isCharged = tag.getBoolean("ch")
        isWorking = tag.getBoolean("work")
        workRemaining = tag.getInteger("rem")
        workMax = tag.getInteger("max")
        loadInv(tag)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
        out.writeBoolean(isWorking)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        isCharged = in.readBoolean()
        isWorking = in.readBoolean()
    }

    override def read(in:MCDataInput, switchkey:Int) = switchkey match
    {
        case 14 =>
            isCharged = in.readBoolean()
            isWorking = in.readBoolean()
            if (hasLight)
            {
                markRender()
                markLight()
            }
        case _ => super.read(in, switchkey)
    }

    def sendWorkUpdate()
    {
        writeStream(14).writeBoolean(isCharged).writeBoolean(isWorking).sendToChunk()
    }

    def canStart = false

    def startWork()
    def endWork()
    {
        isWorking = false
        workRemaining = 0
        workMax = 0
    }

    def produceResults()

    def calcDoableWork = if (cond.canWork) 1 else 0
    def drainPower(work:Int) = cond.drawPower(work*1100.0D)

    override def update()
    {
        super.update()

        if (isWorking)
        {
            if (workRemaining > 0)
            {
                val pow = calcDoableWork
                drainPower(pow)
                workRemaining -= pow
            }
            else
            {
                endWork()
                produceResults()
            }
        }

        if (!isWorking && calcDoableWork > 0 && canStart)
        {
            startWork()
        }

        if (world.getTotalWorldTime%10 == 0) updateRendersIfNeeded()
    }

    override def markDirty()
    {
        super.markDirty()
        if (isWorking && !canStart) endWork()
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, x, y, z)
    }

    def progressScaled(scale:Int):Int =
    {
        if (!isWorking || workMax <= 0 || workRemaining <= 0) return 0
        scale*(workMax-workRemaining)/workMax
    }

    private var oldW = isWorking
    private var oldCh = isCharged
    def updateRendersIfNeeded()
    {
        isCharged = cond.canWork
        if (isWorking != oldW || isCharged != oldCh)
            sendWorkUpdate()

        oldW = isWorking
        oldCh = isCharged
    }

    def hasLight = true

    override def getLightValue = if (isWorking && isCharged) 13 else 0
}

class ContainerPoweredMachine(tile:TPoweredMachine) extends NodeContainer
{
    private var ch = -2
    private var fl = -2

    override def detectAndSendChanges()
    {
        super.detectAndSendChanges()
        import scala.collection.JavaConversions._
        for (i <- crafters)
        {
            val ic = i.asInstanceOf[ICrafting]

            if (ch != tile.cond.charge) ic.sendProgressBarUpdate(this, 0, tile.cond.charge)
            if (fl != tile.cond.flow)
            {
                ic.sendProgressBarUpdate(this, 1, tile.cond.flow&0xFFFF)
                ic.sendProgressBarUpdate(this, 2, tile.cond.flow>>16&0xFFFF)
            }
            ch = tile.cond.charge
            fl = tile.cond.flow
        }
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 0 => tile.cond.charge = bar
        case 1 => tile.cond.flow = tile.cond.flow&0xFFFF0000|bar&0xFFFF
        case 2 => tile.cond.flow = tile.cond.flow&0xFFFF|(bar&0xFFFF)<<16
        case _ => super.updateProgressBar(id, bar)
    }
}

class ContainerProcessingMachine(tile:TileProcessingMachine) extends ContainerPoweredMachine(tile)
{
    private var wr = 0
    private var wm = 0

    override def detectAndSendChanges()
    {
        super.detectAndSendChanges()
        import scala.collection.JavaConversions._
        for (i <- crafters)
        {
            val ic = i.asInstanceOf[ICrafting]

            if (wr != tile.workRemaining) ic.sendProgressBarUpdate(this, 3, tile.workRemaining)
            if (wm != tile.workMax) ic.sendProgressBarUpdate(this, 4, tile.workMax)
        }
        wr = tile.workRemaining
        wm = tile.workMax
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 3 => tile.workRemaining = bar
        case 4 => tile.workMax = bar
        case _ => super.updateProgressBar(id, bar)
    }
}