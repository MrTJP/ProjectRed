package mrtjp.projectred.transportation

import mrtjp.core.item.ItemKey
import mrtjp.core.world.WorldLib
import mrtjp.projectred.api.IConnectable
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.nbt.NBTTagCompound

trait TPressureSubsystem extends PayloadPipePart[PressurePayload]
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

    override def passPayload(r:PressurePayload):Boolean =
    {
        if (passToPressureDevice(r)) return true

        super.passPayload(r)
    }

    def passToPressureDevice(r:PressurePayload) = WorldLib.getTileEntity(world, posOfStraight(r.output)) match
    {
        case pd:TPressureDevice => pd.acceptItem(r, r.output^1)
        case _ => false
    }

    override def createNewPayload(id:Int) = new PressurePayload(id)
}

trait TPressureTube extends TPressureSubsystem with TColourFilterPipe
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

    override def adjustSpeed(r:PressurePayload)
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

    def hasDestination(r:PressurePayload, from:Int):Boolean =
    {
        val dim = (~(1<<from))&openOuts
        if (dim == 0) return false

        PressurePathFinder.clear()
        PressurePathFinder.pipe = this
        PressurePathFinder.item = r.payload.key
        PressurePathFinder.searchDirs = dim
        PressurePathFinder.colour = r.colour
        PressurePathFinder.start()
        val result = PressurePathFinder.invDirs
        PressurePathFinder.clear()
        result != 0
    }

    override def resolveDestination(r:PressurePayload)
    {
        if (Integer.bitCount(openOuts) > 2 || r.travelData == 0)
        {
            val dim = (~(1<<(r.input^1)))&openOuts

            PressurePathFinder.clear()
            PressurePathFinder.pipe = this
            PressurePathFinder.item = r.payload.key
            PressurePathFinder.searchDirs = dim
            PressurePathFinder.colour = r.colour
            PressurePathFinder.start()
            val invDirs = PressurePathFinder.invDirs
            val backlogDirs = PressurePathFinder.backlogDirs
            PressurePathFinder.clear()

            if (invDirs != 0)
            {
                r.output = resolveOutputConflict(invDirs)
                r.travelData = PressurePriority.inventory
            }
            else if (backlogDirs != 0)
            {
                r.output = resolveOutputConflict(backlogDirs)
                r.travelData = PressurePriority.backlog
            }
            else chooseRandomDestination(r)
        }
        else chooseRandomDestination(r)
    }

    override def injectPayload(r:PressurePayload, in:Int)
    {
        super.injectPayload(r, in)
        if (r.travelData == 0)
            r.tickPayloadWander()
    }

    abstract override def discoverStraightOverride(s:Int):Boolean =
    {
        if (super.discoverStraightOverride(s)) return true
        WorldLib.getTileEntity(world, posOfStraight(s)) match
        {
            case sinv:ISidedInventory => sinv.getAccessibleSlotsFromSide(s^1).nonEmpty
            case inv:IInventory => true
            case _ => false
        }
    }

    override def colorExclude = colour == -1
    override def filteredColors = 1<<colour
}

trait TPressureDevice
{
    def canAcceptInput(item:ItemKey, side:Int):Boolean
    def canAcceptBacklog(item:ItemKey, side:Int):Boolean

    def canConnectSide(side:Int):Boolean

    def acceptItem(item:PressurePayload, side:Int):Boolean
}

class PressureTube extends PayloadPipePart[PressurePayload] with TRedstonePipe with TPressureTube

class ResistanceTube extends PayloadPipePart[PressurePayload] with TRedstonePipe with TPressureTube
{
    override def getPathWeight = 256
}