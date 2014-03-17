package mrtjp.projectred.transportation

import java.util._
import mrtjp.projectred.core.PRColors
import mrtjp.projectred.core.utils.{LiteEnumVal, LiteEnumCollector, ItemKeyStack}
import net.minecraft.entity.item.EntityItem
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.common.ForgeDirection
import scala.collection.convert.WrapAsJava
import scala.collection.immutable.BitSet
import scala.collection.{mutable, immutable}

object RoutedPayload
{
    private var maxID = 0

    def apply() =
    {
        if (maxID < Short.MaxValue)
        {
            maxID += 1
            new RoutedPayload(maxID-1)
        }
        else
        {
            maxID = Short.MinValue
            new RoutedPayload(maxID)
        }
    }

    def apply(id:Int) = new RoutedPayload(id)

    def apply(stack:ItemStack) = make(ItemKeyStack.get(stack))

    def apply(stack:ItemKeyStack) = make(stack)

    private def make(stack:ItemKeyStack):RoutedPayload =
    {
        val r = RoutedPayload()
        r.payload = stack
        r
    }
}

class SendPriority(val ident:String, val speed:Float, val boost:Float, val color:Int) extends LiteEnumVal
{
    override def getCollector = SendPriority
}

object SendPriority extends LiteEnumCollector
{
    val WANDERING = new SendPriority("Wandering", 0.02f, 0.05f, PRColors.RED.ordinal)
    val DEFAULT = new SendPriority("Default", 0.05f, 0.10f, PRColors.ORANGE.ordinal())
    val TERMINATED = new SendPriority("Terminated", 0.02f, 0.05f, PRColors.PURPLE.ordinal())
    val PASSIVE = new SendPriority("Passive", 0.10f, 0.20f, PRColors.BLUE.ordinal())
    val ACTIVE = new SendPriority("Active", 0.20f, 0.30f, PRColors.GREEN.ordinal())

    var typeValues =
    {
        val build = new mutable.ArrayBuilder.ofRef[SendPriority]
        for (i <- values) build += i.asInstanceOf[SendPriority]
        build.result()
    }
}

class RoutedPayload(val payloadID:Int)
{
    var payload:ItemKeyStack = null

    var speed = 0.01F
    var progress = 0.00F
    var input = ForgeDirection.UNKNOWN
    var output = ForgeDirection.UNKNOWN
    var isEntering = true
    var parent:FlowingPipePart = null

    def bind(p:FlowingPipePart)
    {
        parent = p
    }

    def reset()
    {
        isEntering = true
        input = ForgeDirection.UNKNOWN
        output = ForgeDirection.UNKNOWN
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

    def isCorrupted = getItemStack == null || getItemStack.stackSize <= 0 || Item.itemsList(getItemStack.itemID) == null

    def canEqual(other: Any) = other.isInstanceOf[RoutedPayload]

    override def equals(other:Any) = other match
    {
        case that:RoutedPayload => (that canEqual this) && payloadID == that.payloadID
        case _ => false
    }

    override def hashCode() = payloadID

    def load(tag:NBTTagCompound)
    {
        progress = tag.getFloat("prog")
        speed = tag.getFloat("speed")
        setItemStack(ItemStack.loadItemStackFromNBT(tag.getCompoundTag("Item")))
        isEntering = tag.getBoolean("isEnt")
        input = ForgeDirection.getOrientation(tag.getInteger("input"))
        output = ForgeDirection.getOrientation(tag.getInteger("output"))
        loadRouting(tag)
    }

    def save(tag:NBTTagCompound)
    {
        tag.setFloat("prog", progress)
        tag.setFloat("speed", speed)
        val nbttagcompound2:NBTTagCompound = new NBTTagCompound
        getItemStack.writeToNBT(nbttagcompound2)
        tag.setCompoundTag("Item", nbttagcompound2)
        tag.setBoolean("isEnt", isEntering)
        tag.setInteger("input", input.ordinal)
        tag.setInteger("output", output.ordinal)
        saveRouting(tag)
    }

    def getEntityForDrop(x:Int, y:Int, z:Int):EntityItem =
    {
        val dir = if (isEntering) input else output
        val prog:Double = progress
        var deltaX = x + 0.5D
        var deltaY = y + 0.25D
        var deltaZ = z + 0.5D
        import ForgeDirection._
        dir match
        {
            case UP => deltaY = (y-0.25D)+prog
            case DOWN => deltaY = (y-0.25D)+(1.0D-prog)
            case SOUTH => deltaZ = z+prog
            case NORTH => deltaZ = z+(1.0D-prog)
            case EAST => deltaX = x+prog
            case WEST => deltaX = x+(1.0D-prog)
            case _ =>
        }

        val item = new EntityItem(parent.world, deltaX, deltaY, deltaZ, payload.makeStack)
        item.motionX = 0
        item.motionY = 0
        item.motionZ = 0
        item.hoverStart = 0

        dir match
        {
            case UP => item.motionY = +speed
            case DOWN => item.motionY = -speed
            case SOUTH => item.motionZ = +speed
            case NORTH => item.motionZ = -speed
            case EAST => item.motionX = +speed
            case WEST => item.motionX = -speed
            case _ =>
        }
        item.delayBeforeCanPickup = 10
        item.lifespan = 1600
        item
    }

    /** Server-side Routing **/
    var destinationIP = -1
    var destinationUUID:UUID = null
    var hasArrived = false
    var travelLog = BitSet()
    var priority = SendPriority.WANDERING

    def setDestination(ip:Int) =
    {
        destinationIP = ip
        val router = RouterServices.getRouter(ip)
        if (router != null) this.destinationUUID = router.getID
        else destinationIP = -1
        this
    }

    def setPriority(p:SendPriority) =
    {
        priority = p
        this
    }

    def resetTrip =
    {
        if (destinationIP > -1)
        {
            val r = RouterServices.getRouter(destinationIP)
            if (r != null)
            {
                val parent = r.getParent
                if (parent.isInstanceOf[IWorldRequester])
                    (parent.asInstanceOf[IWorldRequester]).trackedItemLost(payload)
            }
        }
        destinationIP = -1
        destinationUUID = null
        hasArrived = false
        priority = SendPriority.WANDERING
        this
    }

    def refreshIP()
    {
        val router:Router = RouterServices.getRouter(destinationIP)
        if (router == null || router.getID != destinationUUID) destinationIP = RouterServices.getIPforUUID(destinationUUID)
    }

    def saveRouting(tag:NBTTagCompound) {}

    def loadRouting(tag:NBTTagCompound) {}
}

class PayloadMovement
{
    var delegate = immutable.HashSet[RoutedPayload]()
    var inputQueue = immutable.HashSet[RoutedPayload]()
    var outputQueue = immutable.HashSet[RoutedPayload]()
    private var delay = 0

    def get(id:Int) = delegate.find(r => r.payloadID == id).getOrElse(null)

    def scheduleLoad(item:RoutedPayload)
    {
        delay = 10
        inputQueue += item
    }

    def executeLoad()
    {
        delay -= 1
        if (delay > 0) return

        delegate ++= inputQueue
        inputQueue = immutable.HashSet[RoutedPayload]()
    }

    def exececuteRemove()
    {
        delegate --= outputQueue
        outputQueue = immutable.HashSet[RoutedPayload]()
    }

    def scheduleRemoval(item:RoutedPayload) =
    {
        if (outputQueue.contains(item)) false
        else
        {
            outputQueue += item
            true
        }
    }

    def unscheduleRemoval(item:RoutedPayload) =
    {
        if (outputQueue.contains(item))
        {
            outputQueue -= item
            true
        }
        else false
    }

    def add(e:RoutedPayload)
    {
        delegate += e
    }

    def it = delegate.iterator
    def Jdel = WrapAsJava.asJavaCollection(delegate)
}