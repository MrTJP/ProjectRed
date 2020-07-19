package mrtjp.projectred.transportation

import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.{ItemKey, ItemKeyStack, ItemQueue}

import scala.collection.mutable.ListBuffer

case class LostObj(key:ItemKey, var amount:Int)
{
    var requestAttempts = 0
}

trait TActiveLostStack extends RoutingChip
{
    private var lost = Seq[LostObj]()

    def getMaxRequestAttempts:Int

    def addLostItem(stack:ItemKeyStack)
    {
        lost :+= LostObj(stack.key, stack.stackSize)
    }

    def requestLostItems()
    {
        if (lost.isEmpty) return

        val obj = lost.head
        val LostObj(key, amount) = obj

        val toRequest = math.min(amount, router.getActiveFreeSpace(key))

        val requested = if (toRequest <= 0) 0 else
        {
            val req = new RequestConsole(RequestFlags.full).setDestination(router)
            req.makeRequest(ItemKeyStack.get(key, toRequest)).requested
        }

        if (requested <= 0 && toRequest > 0)
        {
            obj.requestAttempts += 1
            if (obj.requestAttempts > getMaxRequestAttempts)
            {
                lost = lost.tail
                itemLostUnrecoverable(key, toRequest)
            }
            else lost = lost.tail :+ obj
        }
        else
        {
            obj.amount -= requested
            if (obj.amount <= 0)
                lost = lost.tail
        }
    }

    def itemLostUnrecoverable(item:ItemKey, amount:Int)
}

class ChipCrafting extends RoutingChip with TChipCrafter with TChipPriority with TChipMatchMatrix with TActiveBroadcastStack with TActiveLostStack
{
    var excess = new ItemQueue

    private var remainingDelay = operationDelay
    private var remainingDelay2 = operationDelay2

    private def operationDelay = 10
    private def operationDelay2 = 40

    override def getMatchInventory = matrix

    override def getStacksToExtract = 1
    override def getItemsToExtract = 64

    override def timeOutOnFailedExtract = false

    override def getMaxRequestAttempts = 8

    override def extractItem(item:ItemKey, amount:Int) =
    {
        val real = invProvider.getInventory
        if (real != null)
            InvWrapper.wrap(real).setSlotsFromSide(invProvider.getInterfacedSide)
                    .extractItem(item, amount)
        else 0
    }

    override def itemLostUnrecoverable(item:ItemKey, amount:Int)
    {
        val required = getAmountForIngredient(item).toDouble
        if (required > 0)
        {
            val failed = (amount/required).ceil.toInt
            var i = 0
            while (i < failed && hasOrders)
            {
                val BroadcastObject(s, r) = popAll()
                r.postNetworkEvent(TrackedPayloadCancelledEvent(s.key, s.stackSize, router))
                i += 1
            }
        }
    }

    override def onEventReceived(event:NetworkEvent) = event match
    {
        case e:PayloadLostEnrouteEvent if hasOrders && isIngredient(e.item) =>
            addLostItem(ItemKeyStack.get(e.item, e.remaining))
            e.remaining = 0
            e.setCanceled()
        case e:TrackedPayloadCancelledEvent if hasOrders && isIngredient(e.item) =>
            addLostItem(ItemKeyStack.get(e.item, e.remaining))
            e.remaining = 0
            e.setCanceled()
        case _ =>
    }

    override def update()
    {
        remainingDelay -= 1
        if (remainingDelay <= 0)
        {
            remainingDelay = operationDelay
            if (hasOrders)
            {
                RouteFX2.spawnType1(RouteFX2.color_checkInv, router.getPipe)
                doExtractOperation()
            }
            else doExcessExtractOperation()
        }

        remainingDelay2 -= 1
        if (remainingDelay2 <= 0)
        {
            remainingDelay2 = operationDelay2
            requestLostItems()
        }
    }

    def doExcessExtractOperation()
    {
        if (hasOrders) return

        var stacksRemaining = getStacksToExtract
        var itemsRemaining = getItemsToExtract

        val it = excess.result.iterator

        import scala.util.control.Breaks._
        while (it.hasNext && stacksRemaining > 0 && itemsRemaining > 0) breakable
        {
            val (item, amount) = it.next()

            val real = invProvider.getInventory
            if (real == null)
            {
                excess.remove(item, amount)
                break()
            }

            var toExtract = amount
            toExtract = math.min(toExtract, itemsRemaining)
            toExtract = math.min(toExtract, item.getMaxStackSize)

            val removed = extractItem(item, toExtract)
            if (removed <= 0 && timeOutOnFailedExtract)
            {
                excess.remove(item, amount)
                break()
            }

            if (removed > 0)
            {
                router.queueStackToSend(item, removed, Priorities.WANDERING, -1)
                excess.remove(item, removed)
            }

            stacksRemaining -= 1
            itemsRemaining -= removed
        }
    }

    override def getBroadcastPriority = preference

    override def onRemoved()
    {
        while (hasOrders)
        {
            val BroadcastObject(s, r) = popAll()
//            r.itemLost(s)
            r.postNetworkEvent(TrackedPayloadCancelledEvent(s.key, s.stackSize, router))
        }
    }

    override def requestPromise(request:RequestBranchNode, existingPromises:Int)
    {
        if (excess.isEmpty) return
        val itemEq = request.getRequestedPackage

        val craftedItem = getCraftedItem
        if (craftedItem == null || !itemEq.matches(craftedItem.key)) return

        val remaining = excess(itemEq.key)-existingPromises
        if (remaining <= 0) return

        request.addPromise(new DeliveryPromise(craftedItem.key,
            math.min(remaining, request.getMissingCount), router, true, true))
    }

    override def deliverPromise(promise:DeliveryPromise, requester:IRouterContainer)
    {
        val craftedItem = getCraftedItem
        if (craftedItem == null || craftedItem.key != promise.item) return

        if (promise.isExcess) excess.remove(promise.item, promise.size)
        addOrder(ItemKeyStack.get(promise.item, promise.size), requester, Priorities.ACTIVEC)
    }

    override def requestCraftPromise(request:RequestBranchNode) =
    {
        val result = getCraftedItem
        if (result != null && request.getRequestedPackage.matches(result.key))
        {
            val promise = new CraftingPromise(result, router, preference)
            for (i <- 0 until 9)
            {
                val stack = matrix.getStackInSlot(i)
                if (stack != null && stack.stackSize > 0)
                    promise.addIngredient(ItemKeyStack.get(stack), createEqualityFor(i), getCrafterForSlot(i))
            }
            promise
        }
        else null
    }

    def getCrafterForSlot(i:Int):IRouterContainer =
    {
        val s = extMatrix.getStackInSlot(i)
        if (s != null && ItemRoutingChip.hasChipInside(s))
        {
            val c = ItemRoutingChip.loadChipFromItemStack(s).asInstanceOf[TChipCrafterExtension]

            val routers = ChipCraftingExtension.getRoutersForExtension(c.id)
            if (routers.size == 1)
            {
                val r = RouterServices.getRouter(RouterServices.getIPforUUID(routers.head))
                if (r != null && r.isLoaded && r.isInNetwork(router.getRouter.getIPAddress)) r.getContainer
            }
        }
        router
    }

    override def registerExcess(promise:DeliveryPromise)
    {
        if (getCraftedItem.key == promise.item)
            excess += promise.item -> promise.size
    }

    override def getCraftedItem =
    {
        val s = matrix.getStackInSlot(9)
        if (s != null) ItemKeyStack.get(s)
        else null
    }

    override def getProcessingItems = getTotalDeliveryCount

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addMatrixInfo(list)
        addPriorityInfo(list)
        addExtInfo(list)
    }

    def getChipType = RoutingChipDefs.ITEMCRAFTING
}
