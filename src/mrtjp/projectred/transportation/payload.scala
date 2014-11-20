package mrtjp.projectred.transportation

import java.util.UUID
import codechicken.lib.data.MCDataInput
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

    private def claimID() =
    {
        if (maxID < Short.MaxValue) maxID += 1
        else maxID = 0
        maxID
    }

    def create():PipePayload = new PipePayload(claimID())

    def apply(stack:ItemStack):PipePayload = apply(ItemKeyStack.get(stack))
    def apply(stack:ItemKeyStack):PipePayload = apply(claimID(), stack)

    def apply(id:Int, stack:ItemStack):PipePayload = apply(id, ItemKeyStack.get(stack))
    def apply(id:Int, stack:ItemKeyStack):PipePayload =
    {
        val r = new PipePayload(id)
        r.payload = stack.copy
        r
    }
}

class PipePayload(val payloadID:Int)
{
    var payload:ItemKeyStack = null
    var parent:PayloadPipePart = null

    // 0000 NNNN PPPP PPPP SSSS SSSS 0EOO OIII
    // I = input
    // O = output
    // E = isEntering
    // S = speed
    // P = progress
    // N = priority index
    var data = 0

    def isEntering = ((data>>6)&1) != 0
    def isEntering_=(b:Boolean){ if (b) data |= 0x40 else data &= ~0x40 }

    def speed = ((data>>8)&0xFF)/100.0F
    def speed_=(f:Float){ data = data&0xFFFF00FF|((f*100).toInt&0xFF)<<8 }

    def progress = ((data>>16)&0xFF)/100.0F
    def progress_=(f:Float){ data = data&0xFF00FFFF|((f*100).toInt&0xFF)<<16 }

    def input = data&0x7
    def input_=(i:Int){ data = (data& ~0x7)|(i&0x7) }

    def output = (data>>3)&0x7
    def output_=(i:Int){ data = (data& ~0x38)|(i&0x7)<<3 }

    def priorityIndex = (data>>24)&0xF
    def priorityIndex_=(i:Int){ data = (data& ~0xF000000)|(i&0xF)<<24 }

    def bind(p:PayloadPipePart){ parent = p }

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

    override def equals(other:Any) = other match
    {
        case that:PipePayload => payloadID == that.payloadID
        case _ => false
    }

    override def hashCode() = payloadID

    def load(tag:NBTTagCompound)
    {
        if (tag.getBoolean("NoLegacy")) //TODO Legacy
        {
            data = tag.getInteger("idata")
            setItemStack(ItemStack.loadItemStackFromNBT(tag.getCompoundTag("Item")))
            loadRouting(tag)
        }
        else
        {
            progress = tag.getFloat("prog")
            speed = tag.getFloat("speed")
            setItemStack(ItemStack.loadItemStackFromNBT(tag.getCompoundTag("Item")))
            isEntering = tag.getBoolean("isEnt")
            input = tag.getByte("input")
            output = tag.getByte("output")
            loadRouting(tag)
        }
    }

    def save(tag:NBTTagCompound)
    {
        tag.setBoolean("NoLegacy", true) //TODO Legacy
        tag.setInteger("idata", data)
        val tag2 = new NBTTagCompound
        getItemStack.writeToNBT(tag2)
        tag.setTag("Item", tag2)
        saveRouting(tag)
    }

    def getEntityForDrop(x:Int, y:Int, z:Int):EntityItem =
    {
        val dir = if (isEntering) input else output
        val prog = progress
        var deltaX = x+0.5D
        var deltaY = y+0.25D
        var deltaZ = z+0.5D
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

    def getOrElseUpdate(id:Int, f:Unit => PipePayload) =
    {
        val payload = get(id)
        if (payload == null)
        {
            val newInput = f(())
            add(newInput)
            newInput
        }
        else payload
    }

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