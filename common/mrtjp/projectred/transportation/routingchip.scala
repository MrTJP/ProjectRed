package mrtjp.projectred.transportation

import codechicken.core.IGuiPacketSender
import codechicken.core.ServerUtils
import codechicken.lib.packet.PacketCustom
import java.util
import mrtjp.projectred.core.inventory.{InventoryWrapper, SimpleInventory}
import mrtjp.projectred.core.utils.ItemKey
import mrtjp.projectred.core.utils.ItemKeyStack
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip
import mrtjp.projectred.transportation.RequestBranchNode.DeliveryPromise
import mrtjp.projectred.transportation.RoutedPayload.SendPriority
import mrtjp.projectred.transportation.RoutingChipContainerFactory.ChipContainer
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumChatFormatting
import org.lwjgl.input.Keyboard
import scala.collection.convert.WrapAsJava
import scala.collection.mutable.ListBuffer

abstract class RoutingChipset
{
    private var invProv:IInventoryProvider = null
    private var rl:IRouteLayer = null
    private var s = 0

    private val upgrdBus = createUpgradeBus

    def setEnvironment(inventoryProvider:IInventoryProvider, routeLayer:IRouteLayer, slot:Int)
    {
        invProv = inventoryProvider
        rl = routeLayer
        s = slot
    }

    def inventoryProvider = invProv

    def routeLayer = rl

    def slot = s

    def update() {}

    /** Syncing **/
    def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse = null

    /** Broadcasting **/
    def requestPromises(request:RequestBranchNode, existingPromises:Int) {}
    def deliverPromises(promise:DeliveryPromise, requester:IWorldRequester) {}
    def getProvidedItems(map:java.util.Map[ItemKey, Integer]) {}

    def getPriority = Integer.MAX_VALUE
    def getWorkLoad:Double = 0

    /** Requesting **/
    def trackedItemLost(s:ItemKeyStack) {}
    def trackedItemReceived(s:ItemKeyStack) {}

    /** World interactions **/
    def onPipeBroken() {}
    def onNeighborTileChanged(side:Int, weak:Boolean) {}
    def weakTileChanges = false

    def save(tag:NBTTagCompound)
    {
        val tag2:NBTTagCompound = new NBTTagCompound
        upgradeBus.save(tag2)
        tag.setTag("upgrd", tag2)
    }

    def load(tag:NBTTagCompound)
    {
        val tag2:NBTTagCompound = tag.getCompoundTag("upgrd")
        upgradeBus.load(tag2)
    }

    def infoCollection(list:ListBuffer[String])
    {
        addUpgradeBusInfo(list)
    }
    def JInfoCollection(list:util.List[String])
    {
        val l2 = new ListBuffer[String]
        infoCollection(l2)

        val l = WrapAsJava.bufferAsJavaList[String](l2)
        list.addAll(l)
    }

    def getChipType:EnumRoutingChip

    def openGui(player:EntityPlayer)
    {
        if (player.worldObj.isRemote) return
        ServerUtils.openSMPContainer(player.asInstanceOf[EntityPlayerMP], createContainer(player), new IGuiPacketSender
        {
            def sendPacket(player:EntityPlayerMP, windowId:Int)
            {
                val packet:PacketCustom = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Chipset_open)
                packet.writeByte(player.inventory.currentItem)
                packet.writeByte(windowId)
                packet.sendToPlayer(player)
            }
        })
    }

    def createContainer(player:EntityPlayer) =
    {
        val cont = new ChipContainer[RoutingChipset](player)
        cont.addPlayerInventory(8, 86)
        cont
    }

    def createUpgradeBus =
    {
        val bus = new UpgradeBus(0, 0)
        bus
    }

    def upgradeBus = upgrdBus

    def addUpgradeBusInfo(list:ListBuffer[String])
    {
        val list2 = new ListBuffer[String]()
        val b = upgradeBus
        if (b.containsUpgrades)
        {
            list2 += "--- upgrades ---"
            var s:String = ""
            if (b.Lset(0)) s = s + "LX"
            if (b.Lset(1)) s = s + " - LY"
            if (b.Lset(2)) s = s + " - LZ"
            if (!s.isEmpty) list2 += s
            var s2:String = ""
            if (b.Rset(0)) s2 = s2 + "RX"
            if (b.Rset(1)) s2 = s2 + " - RY"
            if (b.Rset(2)) s2 = s2 + " - RZ"
            if (!s2.isEmpty) list2 += s2
            list2 += "----------------"

            for (i <- 0 until list2.length)
                list2.insert(i, EnumChatFormatting.GRAY.toString+list2(i))

            list ++= list2
        }
    }
}

class UpgradeBus(val maxL:Int, val maxR:Int)
{
    var Lset = new Array[Boolean](3)
    var Rset = new Array[Boolean](3)

    var LXLatency = 0
    var LYLatency = 0
    var LZLatency = 0
    var RXLatency = 0
    var RYLatency = 0
    var RZLatency = 0

    var Linfo:String = null
    var Lformula:String = null
    var Rinfo:String = null
    var Rformula:String = null

    def LLatency:Int =
    {
        var count = 0
        if (Lset(0)) count += LXLatency
        if (Lset(1)) count += LYLatency
        if (Lset(2)) count += LZLatency
        return count
    }

    def RLatency:Int =
    {
        var count = 0
        if (Rset(0)) count += RXLatency
        if (Rset(1)) count += RYLatency
        if (Rset(2)) count += RZLatency
        return count
    }

    def setLatency(lx:Int, ly:Int, lz:Int, rx:Int, ry:Int, rz:Int) =
    {
        LXLatency = lx
        LYLatency = ly
        LZLatency = lz
        RXLatency = rx
        RYLatency = ry
        RZLatency = rz
        this
    }

    def installL(i:Int, doInstall:Boolean):Boolean =
    {
        if (i >= maxL) return false
        if (i - 1 >= 0 && !Lset(i - 1)) return false
        if (!Lset(i))
        {
            if (doInstall) Lset(i) = true
            return true
        }
        return false
    }

    def installR(i:Int, doInstall:Boolean):Boolean =
    {
        if (i >= maxR) return false
        if (i - 1 >= 0 && !Rset(i - 1)) return false
        if (!Rset(i))
        {
            if (doInstall) Rset(i) = true
            return true
        }
        return false
    }

    def containsUpgrades:Boolean =
    {
        for (i <- 0 until 3)
            if (Lset(i) || Rset(i)) return true

        return false
    }

    def save(tag:NBTTagCompound)
    {
        for (i <- 0 until 3)
        {
            tag.setBoolean("L"+i, Lset(i))
            tag.setBoolean("R"+i, Rset(i))
        }
    }

    def load(tag:NBTTagCompound)
    {
        for (i <- 0 until 3)
        {
            Lset(i) = tag.getBoolean("L"+i)
            Rset(i) = tag.getBoolean("R"+i)
        }
    }
}

trait TChipFilter extends RoutingChipset
{
    val filter = new SimpleInventory(9, "filter", 1)
    var filterExclude = false

    var fuzzyMode = false
    var fuzzyDamageMode = 0
    val fuzzyPercent = Seq(0, 25, 50, 75, 100)

    // 0-none, 1-type, 2-slot
    var hideMode = 0

    def toggleExcludeMode()
    {
        filterExclude = !filterExclude
    }

    def toggleFuzzyMode()
    {
        fuzzyMode = !fuzzyMode
    }

    def shiftFuzzyDamageMode()
    {
        fuzzyDamageMode = (fuzzyDamageMode+1)%5
    }

    def shiftHiding()
    {
        hideMode = (hideMode+1)%3
    }

    def applyFilter(inv:InventoryWrapper, fuzzy:Boolean=true, hide:Boolean=true):InventoryWrapper =
    {
        if (inv == null) return null

        inv.setSlotsAll()

        if (enableFuzzy && fuzzy)
            inv.setFuzzy(fuzzyMode).setFuzzyPercent(fuzzyPercent(fuzzyDamageMode))

        if (enableHiding && hide) hideMode match
        {
            case 1 => inv.setHidePerType(true)
            case 2 => inv.setHidePerSlot(true)
            case _ =>
        }

        inv
    }

    def enableHiding = true
    def enableFilter = true
    def enableFuzzy = true

    abstract override def save(tag:NBTTagCompound)
    {
        filter.save(tag)
        tag.setBoolean("mode", filterExclude)
        tag.setBoolean("fuz", fuzzyMode)
        tag.setByte("fuzd", fuzzyDamageMode.asInstanceOf[Byte])
        tag.setByte("hide", hideMode.asInstanceOf[Byte])
        super.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        filter.load(tag)
        filterExclude = tag.getBoolean("mode")
        fuzzyMode = tag.getBoolean("fuz")
        fuzzyDamageMode = tag.getByte("fuzd")
        hideMode = tag.getByte("hide")
        super.load(tag)
    }

    val hide = Seq("off", "one per type", "one per stack")
    def addFilterInfo(list:ListBuffer[String])
    {
        if (enableHiding) list+=(EnumChatFormatting.GRAY.toString+"Hide Mode: "+hide(hideMode))

        if (enableFuzzy)
        {
            list+=(EnumChatFormatting.GRAY.toString+"FuzzyMode: "+fuzzyMode)
            list+=(EnumChatFormatting.GRAY.toString+"FuzzyToolDamage: "+fuzzyPercent(fuzzyDamageMode)+"%")
        }

        if (enableFilter)
        {
            list+=(EnumChatFormatting.GRAY.toString+"FilterMode: "+(if (filterExclude) "blacklist" else "whitelist"))
            list+=(EnumChatFormatting.GRAY.toString+"Filter: ")
            var added = false

            for (i <- 0 until filter.getSizeInventory)
            {
                val stack = filter.getStackInSlot(i)
                if (stack != null)
                {
                    list+=(EnumChatFormatting.GRAY.toString+" - "+stack.getDisplayName)
                    added = true
                }
            }

            if (!added) list+=(EnumChatFormatting.GRAY.toString+" - empty")
        }
    }
}

trait TChipPriority extends RoutingChipset
{
    var preference = 0
    var priorityFlag = false

    def sendPriority = SendPriority.PASSIVE

    private def shift = if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) 10 else 1
    def prefUp()
    {
        preference = Math.min(prefScale, preference+shift)
    }

    def prefDown()
    {
        preference = Math.max(-prefScale, preference-shift)
    }

    def prefScale:Int

    def enablePriorityFlag = false

    abstract override def save(tag:NBTTagCompound)
    {
        tag.setInteger("pref", preference)
        super.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        preference = tag.getInteger("pref")
        super.load(tag)
    }

    def addPriorityInfo(list:ListBuffer[String])
    {
        list+=(EnumChatFormatting.GRAY.toString+"Preference: "+preference)
        if (enablePriorityFlag) list+=(EnumChatFormatting.GRAY.toString+"Preference enabled: "+(if (priorityFlag) "yes" else "no"))
    }
}

trait TChipOrientation extends RoutingChipset
{
    var extractOrient = -1

    def side = if (extractOrient <= -1) inventoryProvider.getInterfacedSide else extractOrient

    abstract override def save(tag:NBTTagCompound)
    {
        tag.setInteger("orient", extractOrient)
        super.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        extractOrient = tag.getInteger("orient")
        super.load(tag)
    }

    private val dirs = Seq("Down", "Up", "North", "South", "West", "East")

    def addOrientInfo(list:ListBuffer[String])
    {
        list+=(EnumChatFormatting.GRAY.toString+"Extract Orientation: "+(if (extractOrient == -1) "Default" else dirs(extractOrient)))
    }
}

trait TChipStock extends RoutingChipset
{
    val stock = new SimpleInventory(9, "stock", 127)
    var requestWhenEmpty = false

    def shiftRequestMode()
    {
        requestWhenEmpty = !requestWhenEmpty
    }

    abstract override def save(tag:NBTTagCompound)
    {
        stock.save(tag)
        tag.setBoolean("mode", requestWhenEmpty)
        super.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        stock.load(tag)
        requestWhenEmpty = tag.getBoolean("mode")
        super.load(tag)
    }

    def addStockInfo(list:ListBuffer[String])
    {
        list+=(EnumChatFormatting.GRAY.toString+"Stock: ")
        var added = false
        for (i <- 0 until stock.getSizeInventory)
        {
            val stack = stock.getStackInSlot(i)
            if (stack != null)
            {
                list+=(EnumChatFormatting.GRAY.toString+" - "+stack.getDisplayName+" (" + stack.stackSize + ")")
                added = true
            }
        }
        if (!added) list+=(EnumChatFormatting.GRAY.toString+" - empty")
    }
}

trait TChipCrafter extends RoutingChipset
{
    var matrix = new SimpleInventory(10, "matrix", 127)
    var extIndex = Array[Int](-1, -1, -1, -1, -1, -1, -1, -1, -1)

    def maxExtensions:Int

    def extUp(index:Int)
    {
        if (0 until 9 contains index) extIndex(index) = Math.min(extIndex(index)+1, 8)
    }

    def extDown(index:Int)
    {
        if (0 until 9 contains index) extIndex(index) = Math.max(extIndex(index)-1, -1)
    }

    abstract override def save(tag:NBTTagCompound)
    {
        matrix.save(tag)
        tag.setIntArray("ext", extIndex)
        super.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        matrix.load(tag)
        extIndex = tag.getIntArray("ext")
        super.load(tag)
    }

    def addMatrixInfo(list:ListBuffer[String])
    {
        list += (EnumChatFormatting.GRAY.toString+"Matrix: ")
        var added = false

        for (i <- 0 until 9)
        {
            val stack = matrix.getStackInSlot(i)

            if (stack != null)
            {
                list += (EnumChatFormatting.GRAY.toString+" - "+stack.getDisplayName+" (" + stack.stackSize + ")")
                added = true
            }
        }
        if (!added) list += (EnumChatFormatting.GRAY.toString+" - empty")

        val stack = matrix.getStackInSlot(9)
        if (stack != null) list += (EnumChatFormatting.GRAY.toString+" - Yields: "+stack.getDisplayName+" (" + stack.stackSize + ")")
    }

    def addExtInfo(list:ListBuffer[String])
    {
        list += (EnumChatFormatting.GRAY.toString+"Extensions:")
        for (i <- 0 until 3)
        {
            var s = EnumChatFormatting.GRAY.toString+" - "
            for (j <- 0 until 3)
                s += "["+(if (extIndex(j+(i*3)) >= 0) "+" else "-")+"]"
            list += s
        }
    }
}

