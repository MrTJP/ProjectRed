package mrtjp.projectred.transportation

import java.util.UUID

import com.mojang.realmsclient.gui.ChatFormatting
import mrtjp.core.inventory.{InvWrapper, SimpleInventory}
import mrtjp.core.item.{ItemEquality, ItemKey, ItemKeyStack, ItemQueue}
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.IInventory
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import org.lwjgl.input.Keyboard

import scala.collection.mutable.ListBuffer

abstract class RoutingChip
{
    private var invProv:IInventoryProvider = null
    private var rl:TRouteLayer = null
    private var s = -1

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

    def onEventReceived(event:NetworkEvent){}

    /** Syncing **/
    def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse = null

    /** Broadcasting **/
    def requestPromise(request:RequestBranchNode, existingPromises:Int){}
    def deliverPromise(promise:DeliveryPromise, requester:IWorldRequester){}
    def getBroadcasts(col:ItemQueue){}

    def getBroadcastPriority = Integer.MAX_VALUE
    def getWorkLoad = 0.0D

    /** Crafting **/
    def requestCraftPromise(requeset:RequestBranchNode):CraftingPromise = null
    def registerExcess(promise:DeliveryPromise){}
    def getCraftedItem:ItemKeyStack = null
    def getProcessingItems = 0

    /** World interactions **/
    def onAdded(){}
    def onRemoved(){}
    def onNeighborTileChanged(side:Int, weak:Boolean){}
    def weakTileChanges = false

    def save(tag:NBTTagCompound){}

    def load(tag:NBTTagCompound){}

    def infoCollection(list:ListBuffer[String]){}

    def getChipType:ChipVal

    def openGui(player:EntityPlayer)
    {
        if (player.worldObj.isRemote) return
        GuiChipConfig.open(player, createContainer(player), _.writeByte(player.inventory.currentItem))
    }

    def createContainer(player:EntityPlayer) = new ContainerChipConfig(player, this)
}

abstract class NetworkEvent
{
    private var canceled = false

    def isCanceled = canceled

    def setCanceled()
    {
        if (!isCancelable) throw new Exception(s"Network event ${this.getClass.getSimpleName} cannot be canceled")
        if (canceled) throw new Exception(s"Network event ${this.getClass.getSimpleName} is already canceled")
        canceled = true
    }

    def isCancelable:Boolean
}

class ItemLostEvent(val item:ItemKey, val amount:Int) extends NetworkEvent
{
    var remaining = amount

    override def isCancelable = true
}

class ItemReceivedEvent(val item:ItemKey, val amount:Int) extends NetworkEvent
{
    var remaining = amount

    override def isCancelable = true
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
        filter.saveInv(tag)
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
        filter.loadInv(tag)
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
        if (enableHiding) list+=(ChatFormatting.GRAY.toString+"Hide mode: "+hide(hideMode))

        if (enablePatterns)
        {
            var s = ""
            def sep = if (s == "") "" else ", "
            if (metaMatch) s += "Meta"
            if (nbtMatch) s += sep+"NBT"
            if (oreMatch) s += sep+"Ore Dictionary"
            list+=(ChatFormatting.GRAY.toString+"Matching: "+(if (s.isEmpty) "ignore all" else s))
            if (damageGroupMode!=0)list+=(ChatFormatting.GRAY.toString+"Damage group: "+grpPerc(damageGroupMode)+"%")
        }

        if (enableFilter)
        {
            list+=(ChatFormatting.GRAY.toString+"Filter mode: "+(if (filterExclude) "blacklist" else "whitelist"))
            list+=(ChatFormatting.GRAY.toString+"Filter: ")
            var added = false

            for (i <- 0 until filter.getSizeInventory)
            {
                val stack = filter.getStackInSlot(i)
                if (stack != null)
                {
                    list+=(ChatFormatting.GRAY.toString+" - "+stack.getDisplayName)
                    added = true
                }
            }

            if (!added) list+=(ChatFormatting.GRAY.toString+" - empty")
        }
    }
}

trait TChipPriority extends RoutingChip
{
    var preference = 0

    private def shift = if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) 10 else 1
    def prefUp()
    {
        preference = Math.min(prefScale, preference+shift)
    }

    def prefDown()
    {
        preference = Math.max(-prefScale, preference-shift)
    }

    def prefScale = 32

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
        list+=(ChatFormatting.GRAY.toString+"Preference: "+preference)
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
        list+=(ChatFormatting.GRAY.toString+"Extract orientation: "+(if (extractOrient == -1) "Default" else dirs(extractOrient)))
    }
}

trait TChipStock extends RoutingChip
{
    val stock = new SimpleInventory(9, "stock", 127)
    var requestMode = 0 //0 - stock continuous, 1 - stock empty, 2 - stock infinite

    def shiftRequestMode()
    {
        requestMode = (requestMode+1)%3
    }

    abstract override def save(tag:NBTTagCompound)
    {
        stock.saveInv(tag)
        tag.setByte("rmode", requestMode.toByte)
        super.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        stock.loadInv(tag)
        requestMode = tag.getByte("rmode")
        super.load(tag)
    }

    def addStockInfo(list:ListBuffer[String])
    {
        list += (ChatFormatting.GRAY+"Fill mode: "+(requestMode match
        {
            case 0 => "when missing"
            case 1 => "when empty"
            case 2 => "infinite"
        }))
        list += (ChatFormatting.GRAY.toString+"Stock: ")
        var added = false
        for (i <- 0 until stock.getSizeInventory)
        {
            val stack = stock.getStackInSlot(i)
            if (stack != null)
            {
                list += (ChatFormatting.GRAY.toString+" - "+stack.getDisplayName+" ("+stack.stackSize+")")
                added = true
            }
        }
        if (!added) list += (ChatFormatting.GRAY.toString+" - empty")
    }
}

trait TChipMatchMatrix extends RoutingChip
{
    var matchData = Array.fill[Int](9)(packMatchData(true, true, false, 0))

    val grpPerc = Seq(-1, 25, 50, 75, 99)

    def getMatchInventory:IInventory

    def setData(i:Int, meta:Boolean, nbt:Boolean, ore:Boolean, group:Int)
    {
        matchData(i) = packMatchData(meta, nbt, ore, group)
    }

    def getData(i:Int) = unpackMatchData(matchData(i))

    def packMatchData(meta:Boolean, nbt:Boolean, ore:Boolean, group:Int) =
    {
        var data = 0
        if (meta) data |= 1<<0
        if (nbt) data |= 1<<1
        if (ore) data |= 1<<2
        data |= group<<3
        data
    }

    def unpackMatchData(data:Int) = ((data&1) != 0, (data&2) != 0, (data&4) != 0, data>>3)

    def matchMeta(i:Int) = getData(i)._1
    def matchNBT(i:Int) = getData(i)._2
    def matchOre(i:Int) = getData(i)._3
    def matchGroup(i:Int) = getData(i)._4

    def toggleMatchMeta(i:Int) =
    {
        val (meta, nbt, ore, group) = getData(i)
        setData(i, !meta, nbt, ore, group)
    }
    def toggleMatchNBT(i:Int) =
    {
        val (meta, nbt, ore, group) = getData(i)
        setData(i, meta, !nbt, ore, group)
    }
    def toggleMatchOre(i:Int) =
    {
        val (meta, nbt, ore, group) = getData(i)
        setData(i, meta, nbt, !ore, group)
    }
    def toggleMatchGroup(i:Int) =
    {
        val (meta, nbt, ore, group) = getData(i)
        setData(i, meta, nbt, ore, (group+1)%5)
    }

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setIntArray("matchData", matchData)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        matchData = tag.getIntArray("matchData")
    }

    def createEqualityFor(i:Int) =
    {
        val eq = new ItemEquality
        val (meta, nbt, ore, group) = getData(i)
        eq.setFlags(meta, nbt, ore, group)
        eq
    }
}

trait TChipCrafter extends RoutingChip
{
    var matrix = new SimpleInventory(10, "matrix", 127)
    var extMatrix = new SimpleInventory(9, "ext_matrix", 1)
    {
        override def isItemValidForSlot(slot:Int, stack:ItemStack) =
            stack != null && ItemRoutingChip.hasChipInside(stack) &&
                    RoutingChipDefs.getForStack(stack) == RoutingChipDefs.ITEMEXTENSION
    }

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        matrix.saveInv(tag)
        extMatrix.saveInv(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        matrix.loadInv(tag)
        extMatrix.loadInv(tag)
    }

    def getAmountForIngredient(item:ItemKey) =
    {
        var amount = 0
        for (i <- 0 until 9)
        {
            val s = matrix.getStackInSlot(i)
            if (s != null && ItemKey.get(s) == item)
                amount += s.stackSize
        }
        amount
    }

    def isIngredient(item:ItemKey):Boolean =
    {
        for (i <- 0 until 9)
        {
            val s = matrix.getStackInSlot(i)
            if (s != null && ItemKey.get(s) == item)
                return true
        }
        false
    }

    def addMatrixInfo(list:ListBuffer[String])
    {
        list += (ChatFormatting.GRAY.toString+"Matrix: ")
        var added = false

        for (i <- 0 until 9)
        {
            val stack = matrix.getStackInSlot(i)

            if (stack != null)
            {
                list += (ChatFormatting.GRAY.toString+" - "+stack.getDisplayName+" (" + stack.stackSize + ")")
                added = true
            }
        }
        if (!added) list += (ChatFormatting.GRAY.toString+" - empty")

        val stack = matrix.getStackInSlot(9)
        if (stack != null) list += (ChatFormatting.GRAY.toString+" - Yields: "+stack.getDisplayName+" (" + stack.stackSize + ")")
    }

    def addExtInfo(list:ListBuffer[String])
    {
        list += (ChatFormatting.GRAY.toString+"Extensions: "+
                ChatFormatting.GRAY.toString+(0 until 9).count{extMatrix.getStackInSlot(_) != null})
    }
}

trait TChipCrafterExtension extends RoutingChip
{
    var id = UUID.randomUUID()

    override def save(tag:NBTTagCompound)
    {
        tag.setString("extid", id.toString)
    }

    override def load(tag:NBTTagCompound)
    {
        id = UUID.fromString(tag.getString("extid"))
    }

    def randomizeUUID()
    {
        id = UUID.randomUUID()
    }

    def addExtIDInfo(list:ListBuffer[String])
    {
        list += ChatFormatting.GRAY.toString+"Extension ID: "+id.toString.split("-")(0)+" ..."
    }
}