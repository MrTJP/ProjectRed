package mrtjp.projectred.transportation

import mrtjp.projectred.core.libmc.inventory.{InvWrapper, SimpleInventory}
import mrtjp.projectred.core.libmc.{ItemQueue, ItemKey, ItemKeyStack}
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumChatFormatting
import org.lwjgl.input.Keyboard

import scala.collection.immutable.HashMap
import scala.collection.mutable.{ListBuffer, Builder => MBuilder}

abstract class RoutingChip
{
    private var invProv:IInventoryProvider = null
    private var rl:TRouteLayer = null
    private var s = -1

    private val upgrdBus = createUpgradeBus

    def setEnvironment(inventoryProvider:IInventoryProvider, routeLayer:TRouteLayer, slot:Int)
    {
        invProv = inventoryProvider
        rl = routeLayer
        s = slot
    }

    def invProvider = invProv
    def routeLayer = rl
    def slot = s

    def update(){}

    /** Syncing **/
    def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse = null

    /** Broadcasting **/
    def requestPromises(request:RequestBranchNode, existingPromises:Int){}
    def deliverPromises(promise:DeliveryPromise, requester:IWorldRequester){}
    def getProvidedItems(col:ItemQueue){}

    def getBroadcastPriority = Integer.MAX_VALUE
    def getWorkLoad = 0.0D

    /** Requesting **/
    def trackedItemLost(s:ItemKeyStack){}
    def trackedItemReceived(s:ItemKeyStack){}

    /** World interactions **/
    def onPipeBroken(){}
    def onNeighborTileChanged(side:Int, weak:Boolean){}
    def weakTileChanges = false

    def save(tag:NBTTagCompound)
    {
        val tag2 = new NBTTagCompound
        upgradeBus.save(tag2)
        tag.setTag("upgrd", tag2)
    }

    def load(tag:NBTTagCompound)
    {
        val tag2 = tag.getCompoundTag("upgrd")
        upgradeBus.load(tag2)
    }

    def infoCollection(list:ListBuffer[String])
    {
        addUpgradeBusInfo(list)
    }

    def getChipType:ChipVal

    def openGui(player:EntityPlayer)
    {
        if (player.worldObj.isRemote) return
        ChipGuiFactory.open(player, createContainer(player), _.writeByte(player.inventory.currentItem))
    }

    def createContainer(player:EntityPlayer) =
    {
        val cont = new ChipContainer(player)
        cont.addPlayerInv(player, 8, 86)
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

            list2.transform(s => EnumChatFormatting.GRAY+s)

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
        count
    }

    def RLatency:Int =
    {
        var count = 0
        if (Rset(0)) count += RXLatency
        if (Rset(1)) count += RYLatency
        if (Rset(2)) count += RZLatency
        count
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
        false
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
        false
    }

    def containsUpgrades:Boolean =
    {
        for (i <- 0 until 3)
            if (Lset(i) || Rset(i)) return true

        false
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

trait TChipFilter extends RoutingChip
{
    val filter = new SimpleInventory(9, "filter", 1)
    var filterExclude = false

    var metaMatch = true
    var nbtMatch = true
    var oreMatch = false

    var damageGroupMode = 0
    val grpPerc = Seq(-1, 25, 50, 75, 99)

    // 0-none, 1-type, 2-slot
    var hideMode = 0

    def toggleExcludeMode()
    {
        filterExclude = !filterExclude
    }

    def toggleMetaMode()
    {
        metaMatch = !metaMatch
    }

    def toggleNBTMode()
    {
        nbtMatch = !nbtMatch
    }

    def toggleOreMode()
    {
        oreMatch = !oreMatch
    }

    def shiftDamageGroup()
    {
        damageGroupMode = (damageGroupMode+1)%5
    }

    def shiftHiding()
    {
        hideMode = (hideMode+1)%3
    }

    def applyFilter(inv:InvWrapper, patterns:Boolean=true, hide:Boolean=true):InvWrapper =
    {
        if (inv == null) return null

        inv.setSlotsAll()

        if (enablePatterns && patterns)
            inv.setMatchOptions(metaMatch, nbtMatch, oreMatch).setDamageGroup(grpPerc(damageGroupMode))

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
    def enablePatterns = true

    abstract override def save(tag:NBTTagCompound)
    {
        filter.save(tag)
        tag.setBoolean("mode", filterExclude)
        tag.setBoolean("mm", metaMatch)
        tag.setBoolean("nbtm", nbtMatch)
        tag.setBoolean("om", oreMatch)
        tag.setByte("grp", damageGroupMode.asInstanceOf[Byte])
        tag.setByte("hide", hideMode.asInstanceOf[Byte])
        super.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        filter.load(tag)
        filterExclude = tag.getBoolean("mode")
        metaMatch = tag.getBoolean("mm")
        nbtMatch = tag.getBoolean("nbtm")
        oreMatch = tag.getBoolean("om")
        damageGroupMode = tag.getByte("grp")
        hideMode = tag.getByte("hide")
        super.load(tag)
    }

    val hide = Seq("off", "one per type", "one per stack")
    def addFilterInfo(list:ListBuffer[String])
    {
        if (enableHiding) list+=(EnumChatFormatting.GRAY.toString+"Hide mode: "+hide(hideMode))

        if (enablePatterns)
        {
            var s = ""
            def sep = if (s == "") "" else ", "
            if (metaMatch) s += "Meta"
            if (nbtMatch) s += sep+"NBT"
            if (oreMatch) s += sep+"Ore Dictionary"
            list+=(EnumChatFormatting.GRAY.toString+"Matching: "+(if (s.isEmpty) "ignore all" else s))
            if (damageGroupMode!=0)list+=(EnumChatFormatting.GRAY.toString+"Damage group: "+grpPerc(damageGroupMode)+"%")
        }

        if (enableFilter)
        {
            list+=(EnumChatFormatting.GRAY.toString+"Filter mode: "+(if (filterExclude) "blacklist" else "whitelist"))
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

trait TChipPriority extends RoutingChip
{
    var preference = 0
    var priorityFlag = false

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

trait TChipOrientation extends RoutingChip
{
    var extractOrient = -1

    def side = if (extractOrient <= -1) invProvider.getInterfacedSide else extractOrient

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
        list+=(EnumChatFormatting.GRAY.toString+"Extract orientation: "+(if (extractOrient == -1) "Default" else dirs(extractOrient)))
    }
}

trait TChipStock extends RoutingChip
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
        list+=(EnumChatFormatting.GRAY+"Fill mode: when "+(if(requestWhenEmpty) "empty" else "missing"))
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

trait TChipCrafter extends RoutingChip
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

    def getCraftedItem = ItemKeyStack.get(matrix.getStackInSlot(9))

    def buildCraftPromise(item:ItemKey, pipe:RoutedCraftingPipePart) =
    {
        val result = ItemKeyStack.get(matrix.getStackInSlot(9))
        if (result != null && result.key == item)
        {
            val promise = new CraftingPromise(result, pipe, pipe.priority)
            for (i <- 0 until 9)
            {
                val keystack = ItemKeyStack.get(matrix.getStackInSlot(i))
                if (keystack != null && keystack.stackSize > 0)
                    promise.addIngredient(keystack, pipe.getExtensionFor(extIndex(i)))
            }
            promise
        }
        else null
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