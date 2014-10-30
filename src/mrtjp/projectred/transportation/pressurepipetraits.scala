package mrtjp.projectred.transportation

import mrtjp.core.color.Colors
import mrtjp.core.item.ItemKey
import mrtjp.core.world.WorldLib
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.illumination.LightObjCage
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition

trait TPressureSubsystem extends PayloadPipePart
{
    override def canConnectPart(part:IConnectable, s:Int) = part match
    {
        case p:TPressureSubsystem => true
        case _ => super.canConnectPart(part, s)
    }
}

trait TPressureTube extends TPressureSubsystem
{
    var lastFlow = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("flow", lastFlow.asInstanceOf[Byte])
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        lastFlow = tag.getByte("flow")
    }

    def openOuts = clientConnMap

    override def adjustSpeed(r:PipePayload)
    {
        r.speed = 0.045f
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

    override def resolveDestination(r:PipePayload)
    {
        if (Integer.bitCount(openOuts) > 2 || r.priorityIndex == 0)
        {
            val dim = (~(1<<(r.input^1)))&openOuts

            val pf = new PressurePathfinder(r.payload.key, this, dim)
            pf.start()

            if (pf.invDirs != 0)
            {
                r.output = resolveOutputConflict(pf.invDirs)
                r.priorityIndex == PressurePriority.inventory
            }
            else if (pf.backlogDirs != 0)
            {
                r.output = resolveOutputConflict(pf.invDirs)
                r.priorityIndex == PressurePriority.backlog
            }
            else chooseRandomDestination(r)
        }
        else chooseRandomDestination(r)
    }

    abstract override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true
        injectPayload(PipePayload(LightObjCage.makeStack(Colors.RED.ordinal())), 1)
        true
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
    def acceptInput(item:ItemKey, side:Int):Boolean

    def acceptBacklog(item:ItemKey, side:Int):Boolean
}

class PressureTube extends BasicPipeAbstraction with TPressureTube

class ResistanceTube extends BasicPipeAbstraction with TPressureTube
{
    override def pathWeight = 256
}