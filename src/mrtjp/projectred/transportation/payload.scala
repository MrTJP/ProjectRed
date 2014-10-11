package mrtjp.projectred.transportation

import java.util.UUID

import mrtjp.projectred.core.lib.Enum
import mrtjp.projectred.core.libmc.{ItemKeyStack, PRColors}
import net.minecraft.entity.item.EntityItem
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.common.util.ForgeDirection

import scala.collection.convert.WrapAsJava
import scala.collection.immutable
import scala.collection.immutable.BitSet

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
    var input = ForgeDirection.UNKNOWN
    var output = ForgeDirection.UNKNOWN
    var isEntering = true
    var parent:PayloadPipePart = null

    def bind(p:PayloadPipePart)
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
        input = ForgeDirection.getOrientation(tag.getInteger("input"))
        output = ForgeDirection.getOrientation(tag.getInteger("output"))
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
        import net.minecraftforge.common.util.ForgeDirection._
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
    var priority = Priorities.WANDERING

    def setDestination(ip:Int) =
    {
        destinationIP = ip
        val router = RouterServices.getRouter(ip)
        if (router != null) destinationUUID = router.getID
        else destinationIP = -1
        this
    }

    def setPriority(p:Priorities.SendPriority) =
    {
        priority = p
        this
    }

    def resetTrip =
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
        priority = Priorities.WANDERING
        this
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
    var delegate = immutable.HashSet[PipePayload]()
    var inputQueue = immutable.HashSet[PipePayload]()
    var outputQueue = immutable.HashSet[PipePayload]()
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
        inputQueue = immutable.HashSet[PipePayload]()
    }

    def exececuteRemove()
    {
        delegate --= outputQueue
        outputQueue = immutable.HashSet[PipePayload]()
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

object Priorities extends Enum
{
    type EnumVal = SendPriority

    val passiveDef = {path:StartEndPath => path.allowRouting}
    val activeDef = {path:StartEndPath => path.allowBroadcast || path.allowCrafting}

    val WANDERING = new SendPriority("Wandering", 0.02f, 0.05f, PRColors.RED.ordinal)
    val DEFAULT = new SendPriority("Default", 0.05f, 0.10f, PRColors.ORANGE.ordinal())
    val TERMINATED = new SendPriority("Terminated", 0.02f, 0.05f, PRColors.PURPLE.ordinal())
    val PASSIVE = new SendPriority("Passive", 0.10f, 0.20f, PRColors.BLUE.ordinal())
    val ACTIVEB = new SendPriority("Active Broadcast", 0.20f, 0.30f, PRColors.GREEN.ordinal(), _.allowBroadcast)
    val ACTIVEC = new SendPriority("Active Craft", 0.20f, 0.30f, PRColors.GREEN.ordinal(), _.allowCrafting)


    class SendPriority(val ident:String, val speed:Float, val boost:Float, val color:Int, f:StartEndPath => Boolean) extends this.Value
    {
        def this(ident:String, speed:Float, boost:Float, color:Int) = this(ident, speed, boost, color, passiveDef)

        override def name = ident

        /**
         * Used to check if a particular router can route to another on this priority
         * with the given path. This should see if said path does not restrict this
         * priority. (Item checks are done on the fly, ignore them)
         * @param path The path to check routing for
         * @return True if this priority can route using given path.
         */
        def isPathUsable(path:StartEndPath) = f(path)
    }
}