package mrtjp.projectred.transportation

import java.util.UUID
import codechicken.lib.data.{MCDataOutput, MCDataInput}
import mrtjp.core.item.ItemKeyStack
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.transportation.Priorities.NetworkPriority
import net.minecraft.entity.item.EntityItem
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.common.util.ForgeDirection

import scala.collection.convert.WrapAsJava
import scala.collection.immutable.{BitSet, HashSet}

object AbstractPipePayload
{
    private var maxID = 0

    def claimID() =
    {
        if (maxID < Short.MaxValue) maxID += 1
        else maxID = 0
        maxID
    }

    def create():NetworkPayload = new NetworkPayload(claimID())

//    def apply(stack:ItemStack):NetworkPayload = apply(ItemKeyStack.get(stack))
//    def apply(stack:ItemKeyStack):NetworkPayload = apply(claimID(), stack)
//
//    def apply(id:Int, stack:ItemStack):NetworkPayload = apply(id, ItemKeyStack.get(stack))
//    def apply(id:Int, stack:ItemKeyStack):NetworkPayload =
//    {
//        val r = new NetworkPayload(id)
//        r.payload = stack.copy
//        r
//    }
}

class AbstractPipePayload(val payloadID:Int)
{
    var payload:ItemKeyStack = null
    var parent:PayloadPipePart[_] = null

    // 0000 0000 PPPP PPPP SSSS SSSS 0EOO OIII
    // I = input
    // O = output
    // E = isEntering
    // S = speed
    // P = progress
    var data = 0

    private var wanderThroughs = 0
    def tickPayloadWander(){
        if (Configurator.maxPipesWandered > 0)
            wanderThroughs += 1
    }

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

    def bind(p:PayloadPipePart[_]){ parent = p }

    def reset()
    {
        isEntering = true
        input = 6
        output = 6
    }

    def preItemRemove(){}

    def moveProgress(prog:Float)
    {
        progress += prog
    }

    def getItemStack = payload.makeStack

    def setItemStack(item:ItemStack)
    {
        payload = ItemKeyStack.get(item)
    }

    def isCorrupted = getItemStack == null || getItemStack.stackSize <= 0 ||
            (Configurator.maxPipesWandered > 0 && wanderThroughs > Configurator.maxPipesWandered)

    override def equals(other:Any) = other match
    {
        case that:AbstractPipePayload => payloadID == that.payloadID
        case _ => false
    }

    override def hashCode() = payloadID

    def save(tag:NBTTagCompound)
    {
        tag.setBoolean("NoLegacy", true) //TODO Legacy
        tag.setInteger("idata", data)
        val tag2 = new NBTTagCompound
        getItemStack.writeToNBT(tag2)
        tag.setTag("Item", tag2)
    }

    def load(tag:NBTTagCompound)
    {
        if (tag.getBoolean("NoLegacy")) //TODO Legacy
        {
            data = tag.getInteger("idata")
            setItemStack(ItemStack.loadItemStackFromNBT(tag.getCompoundTag("Item")))
        }
        else
        {
            progress = tag.getFloat("prog")
            speed = tag.getFloat("speed")
            setItemStack(ItemStack.loadItemStackFromNBT(tag.getCompoundTag("Item")))
            isEntering = tag.getBoolean("isEnt")
            input = tag.getByte("input")
            output = tag.getByte("output")
        }
    }

    def writeDesc(packet:MCDataOutput)
    {
        packet.writeItemStack(getItemStack)
        packet.writeInt(data)
    }

    def readDesc(packet:MCDataInput)
    {
        setItemStack(packet.readItemStack())
        data = packet.readInt()
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
}

class PressurePayload(payloadID:Int) extends AbstractPipePayload(payloadID)
{
    // Extended Data
    // CCCC CCCC PPPP PPPP SSSS SSSS 0EOO OIII
    // I = input
    // O = output
    // E = isEntering
    // S = speed
    // P = progress
    // C = colour ******
    def travelData = data>>>24
    def travelData_=(i:Int){ data = (data& ~0xFF000000)|i<<24 }

    var colour:Byte = -1

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("col", colour)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        colour = tag.getByte("col")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(colour)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        colour = packet.readByte()
    }
}

class NetworkPayload(payloadID:Int) extends AbstractPipePayload(payloadID)
{
    // Extended Data
    // 0000 NNNN PPPP PPPP SSSS SSSS 0EOO OIII
    // I = input
    // O = output
    // E = isEntering
    // S = speed
    // P = progress
    // N = priority index ******
    def priorityIndex = (data>>24)&0xF
    def priorityIndex_=(i:Int){ data = (data& ~0xF000000)|(i&0xF)<<24 }

    override def preItemRemove()
    {
        resetTrip()
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
}

class PayloadMovement[T <: AbstractPipePayload]
{
    var delegate = HashSet[T]()
    var inputQueue = HashSet[T]()
    var outputQueue = HashSet[T]()
    private var delay = 0

    private implicit def payloadToT(p:AbstractPipePayload):T = p.asInstanceOf[T]

    def get(id:Int):T = delegate.find(_.payloadID == id).getOrElse(null.asInstanceOf[T])

    def getOrElseUpdate(id:Int, f:Unit => T):T =
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

    def scheduleLoad(item:T)
    {
        delay = 10
        inputQueue += item
    }

    def executeLoad()
    {
        delay -= 1
        if (delay > 0) return

        delegate ++= inputQueue
        inputQueue = HashSet[T]()
    }

    def exececuteRemove()
    {
        delegate --= outputQueue
        outputQueue = HashSet[T]()
    }

    def scheduleRemoval(item:T) =
    {
        if (outputQueue.contains(item)) false
        else
        {
            outputQueue += item
            true
        }
    }

    def unscheduleRemoval(item:T) =
    {
        if (outputQueue.contains(item))
        {
            outputQueue -= item
            true
        }
        else false
    }

    def add(e:T)
    {
        delegate += e
    }

    def it = delegate.iterator
    def Jdel = WrapAsJava.asJavaCollection(delegate)
}