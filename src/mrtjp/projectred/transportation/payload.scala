package mrtjp.projectred.transportation

import java.util.UUID
import mrtjp.core.item.ItemKeyStack
import mrtjp.projectred.transportation.Priorities.NetworkPriority
import net.minecraft.entity.item.EntityItem
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.common.util.ForgeDirection

import scala.collection.convert.WrapAsJava
import scala.collection.immutable.{BitSet, HashSet}

object PipePayload
{
    private var maxID = 0

    def apply() =
    {
        if (maxID < Short.MaxValue)
        {
            maxID += 1
            new PipePayload(maxID-1)
        }
        else
        {
            maxID = Short.MinValue
            new PipePayload(maxID)
        }
    }

    def apply(id:Int) = new PipePayload(id)

    def apply(stack:ItemStack) = make(ItemKeyStack.get(stack))

    def apply(stack:ItemKeyStack) = make(stack)

    private def make(stack:ItemKeyStack):PipePayload =
    {
        val r = PipePayload()
        r.payload = stack
        r
    }
}

class PipePayload(val payloadID:Int)
{
    var payload:ItemKeyStack = null

    var speed = 0.01F
    var progress = 0.00F
    var input = 6
    var output = 6
    var isEntering = true
    var parent:PayloadPipePart = null
    var priorityIndex = 0

    def bind(p:PayloadPipePart)
    {
        parent = p
    }

    def reset()
    {
        isEntering = true
        input = 6
        output = 6
    }

    def moveProgress(prog:Float)
    {
        progress += prog
    }

    def getItemStack = payload.makeStack

    def setItemStack(item:ItemStack)
    {
        payload = ItemKeyStack.get(item)
    }

    def isCorrupted = getItemStack == null || getItemStack.stackSize <= 0

    def canEqual(other: Any) = other.isInstanceOf[PipePayload]

    override def equals(other:Any) = other match
    {
        case that:PipePayload => (that canEqual this) && payloadID == that.payloadID
        case _ => false
    }

    override def hashCode() = payloadID

    def load(tag:NBTTagCompound)
    {
        progress = tag.getFloat("prog")
        speed = tag.getFloat("speed")
        setItemStack(ItemStack.loadItemStackFromNBT(tag.getCompoundTag("Item")))
        isEntering = tag.getBoolean("isEnt")
        input = tag.getByte("input")
        output = tag.getByte("output")
        loadRouting(tag)
    }

    def save(tag:NBTTagCompound)
    {
        tag.setFloat("prog", progress)
        tag.setFloat("speed", speed)
        val tag2 = new NBTTagCompound
        getItemStack.writeToNBT(tag2)
        tag.setTag("Item", tag2)
        tag.setBoolean("isEnt", isEntering)
        tag.setByte("input", input.asInstanceOf[Byte])
        tag.setByte("output", output.asInstanceOf[Byte])
        saveRouting(tag)
    }

    def getEntityForDrop(x:Int, y:Int, z:Int):EntityItem =
    {
        val dir = if (isEntering) input else output
        val prog:Double = progress
        var deltaX = x + 0.5D
        var deltaY = y + 0.25D
        var deltaZ = z + 0.5D
        dir match
        {
            case 0 => deltaY = (y-0.25D)+(1.0D-prog)
            case 1 => deltaY = (y-0.25D)+prog
            case 2 => deltaZ = z+(1.0D-prog)
            case 3 => deltaZ = z+prog
            case 4 => deltaX = x+(1.0D-prog)
            case 5 => deltaX = x+prog
            case _ =>
        }

        val item = new EntityItem(parent.world, deltaX, deltaY, deltaZ, payload.makeStack)
        item.motionX = 0
        item.motionY = 0
        item.motionZ = 0
        item.hoverStart = 0

        dir match
        {
            case 0 => item.motionY = -speed
            case 1 => item.motionY = +speed
            case 2 => item.motionZ = -speed
            case 3 => item.motionZ = +speed
            case 4 => item.motionX = -speed
            case 5 => item.motionX = +speed
            case _ =>
        }
        item.delayBeforeCanPickup = 10
        item.lifespan = 1600
        item
    }

    /** Server-side Routing, used if moving through network pipes **/
    var destinationIP = -1
    var destinationUUID:UUID = null
    var hasArrived = false
    var travelLog = BitSet()

    def netPriority = Priorities(priorityIndex)

    def setDestination(ip:Int, p:NetworkPriority) =
    {
        destinationIP = ip
        priorityIndex = p.ordinal
        val router = RouterServices.getRouter(ip)
        if (router != null) destinationUUID = router.getID
        else destinationIP = -1

        this
    }

    def resetTrip()
    {
        if (destinationIP > -1)
        {
            val r = RouterServices.getRouter(destinationIP)
            if (r != null) r.getParent match
            {
                case wr:IWorldRequester => wr.trackedItemLost(payload)
                case _ =>
            }
        }
        destinationIP = -1
        destinationUUID = null
        hasArrived = false
        priorityIndex = Priorities.WANDERING.ordinal
    }

    def refreshIP()
    {
        val router = RouterServices.getRouter(destinationIP)
        if (router == null || router.getID != destinationUUID) destinationIP = RouterServices.getIPforUUID(destinationUUID)
    }

    def saveRouting(tag:NBTTagCompound) {}

    def loadRouting(tag:NBTTagCompound) {}
}

class PayloadMovement
{
    var delegate = HashSet[PipePayload]()
    var inputQueue = HashSet[PipePayload]()
    var outputQueue = HashSet[PipePayload]()
    private var delay = 0

    def get(id:Int) = delegate.find(_.payloadID == id).orNull

    def scheduleLoad(item:PipePayload)
    {
        delay = 10
        inputQueue += item
    }

    def executeLoad()
    {
        delay -= 1
        if (delay > 0) return

        delegate ++= inputQueue
        inputQueue = HashSet[PipePayload]()
    }

    def exececuteRemove()
    {
        delegate --= outputQueue
        outputQueue = HashSet[PipePayload]()
    }

    def scheduleRemoval(item:PipePayload) =
    {
        if (outputQueue.contains(item)) false
        else
        {
            outputQueue += item
            true
        }
    }

    def unscheduleRemoval(item:PipePayload) =
    {
        if (outputQueue.contains(item))
        {
            outputQueue -= item
            true
        }
        else false
    }

    def add(e:PipePayload)
    {
        delegate += e
    }

    def it = delegate.iterator
    def Jdel = WrapAsJava.asJavaCollection(delegate)
}