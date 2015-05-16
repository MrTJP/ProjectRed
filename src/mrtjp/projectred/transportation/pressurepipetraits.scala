package mrtjp.projectred.transportation

import mrtjp.core.item.ItemKey
import mrtjp.core.world.WorldLib
import mrtjp.projectred.api.IConnectable
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.nbt.NBTTagCompound

trait TPressureSubsystem extends PayloadPipePart
{
    abstract override def canConnectPart(part:IConnectable, s:Int) = part match
    {
        case ps:TPressureSubsystem => true
        case pd:TPressureDevice if pd.canConnectSide(s^1) => true
        case _ => super.canConnectPart(part, s)
    }

    abstract override def discoverStraightOverride(s:Int) =
    {
        WorldLib.getTileEntity(world, posOfStraight(s)) match
        {
            case pd:TPressureDevice if pd.canConnectSide(s^1) => true
            case _ => super.discoverStraightOverride(s)
        }
    }

    override def passPayload(r:PipePayload):Boolean =
    {
        if (passToPressureDevice(r)) return true

        super.passPayload(r)
    }

    def passToPressureDevice(r:PipePayload) = WorldLib.getTileEntity(world, posOfStraight(r.output)) match
    {
        case pd:TPressureDevice => pd.acceptItem(r, r.output^1)
        case _ => false
    }
}

trait TPressureTube extends TPressureSubsystem
{
    var lastFlow = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("flow", lastFlow.toByte)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        lastFlow = tag.getByte("flow")
    }

    def openOuts = clientConnMap

    override def adjustSpeed(r:PipePayload)
    {
        r.speed = 0.05f
    }

    def resolveOutputConflict(outs:Int):Int =
    {
        import java.lang.Integer.{bitCount => count, numberOfTrailingZeros => trail}

        val out2 = outs&openOuts&0x3F
        if (count(out2) == 1)
        {
            val dest = trail(out2)
            lastFlow = 1<<((dest+1)%6)
            return dest
        }
        else if (count(out2) > 1)
        {
            val strip = trail(lastFlow)
            val dest = (trail((out2<<6|out2)>>strip)+strip)%6
            lastFlow = 1<<((dest+1)%6)
            return dest
        }
        trail(lastFlow)
    }

    def hasDestination(r:PipePayload, from:Int):Boolean =
    {
        val dim = (~(1<<from))&openOuts
        if (dim == 0) return false

        val pf = new PressurePathfinder(r.payload.key, this, dim) //TODO incorporate color
        pf.start()
        pf.invDirs != 0
    }

    override def resolveDestination(r:PipePayload)
    {
        if (Integer.bitCount(openOuts) > 2 || r.priorityIndex == 0)
        {
            val dim = (~(1<<(r.input^1)))&openOuts

            val pf = new PressurePathfinder(r.payload.key, this, dim) //TODO incorporate color
            pf.start()

            if (pf.invDirs != 0)
            {
                r.output = resolveOutputConflict(pf.invDirs)
                r.priorityIndex == PressurePriority.inventory
            }
            else if (pf.backlogDirs != 0)
            {
                r.output = resolveOutputConflict(pf.backlogDirs)
                r.priorityIndex == PressurePriority.backlog
            }
            else chooseRandomDestination(r)
        }
        else chooseRandomDestination(r)
    }

    abstract override def discoverStraightOverride(s:Int):Boolean =
    {
        WorldLib.getTileEntity(world, posOfStraight(s)) match
        {
            case sinv:ISidedInventory => sinv.getAccessibleSlotsFromSide(s^1).nonEmpty
            case inv:IInventory => true
            case _ => super.discoverStraightOverride(s)
        }
    }
}

trait TPressureDevice
{
    def canAcceptInput(item:ItemKey, side:Int):Boolean
    def canAcceptBacklog(item:ItemKey, side:Int):Boolean

    def canConnectSide(side:Int):Boolean

    def acceptItem(item:PipePayload, side:Int):Boolean
}

class PressureTube extends BasicPipeAbstraction with TPressureTube

class ResistanceTube extends BasicPipeAbstraction with TPressureTube
{
    override def pathWeight = 256
}