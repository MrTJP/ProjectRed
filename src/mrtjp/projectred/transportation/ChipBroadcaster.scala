package mrtjp.projectred.transportation

import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.{ItemKey, ItemKeyStack, ItemQueue}
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal

import scala.collection.mutable.ListBuffer

case class BroadcastObject(stack:ItemKeyStack, requester:IWorldRequester)
{
    var priority:Priorities.Priority = null
}

trait TActiveBroadcastStack extends RoutingChip
{
    private var orders = Seq[BroadcastObject]()

    def addOrder(stack:ItemKeyStack, requester:IWorldRequester, priority:Priorities.Priority)
    {
        orders.find(p => p.stack.key == stack.key && p.requester == requester) match
        {
            case Some(p) =>
                p.stack.stackSize += stack.stackSize
                val idx = orders.indexOf(p)
                orders = orders.take(idx) ++ orders.drop(idx+1) :+ p
            case _ =>
                val b = BroadcastObject(stack, requester)
                b.priority = priority
                orders :+= b
        }
        onOrdersChanged()
    }

    def pop(amount:Int) =
    {
        val BroadcastObject(stack, _) = orders.head
        stack.stackSize -= amount
        if (stack.stackSize <= 0)
        {
            orders = orders.tail
            true
        }
        else false
    }

    def popAll() =
    {
        val out = orders.head
        orders = orders.tail
        out
    }

    def restackOrders()
    {
        orders = orders.tail :+ orders.head
    }

    def peek =
        if (orders.isEmpty) null
        else orders.head

    def hasOrders = orders.nonEmpty

    def getDeliveryCount(item:ItemKey) = orders.foldLeft(0)(
        (b, p) => b+(if (p.stack.key == item) p.stack.stackSize else 0))

    def getTotalDeliveryCount = orders.foldLeft(0)((b, p) => b+p.stack.stackSize)

    def onOrdersChanged(){}

    def getStacksToExtract:Int

    def getItemsToExtract:Int

    def extractItem(item:ItemKey, amount:Int):Int

    def timeOutOnFailedExtract:Boolean

    def doExtractOperation()
    {
        if (!hasOrders) return

        var stacksRemaining = getStacksToExtract
        var itemsRemaining = getItemsToExtract

        val wh, cont = new scala.util.control.Breaks
        wh.breakable {
            while (hasOrders && stacksRemaining > 0 && itemsRemaining > 0) cont.breakable
            {
                val bObj = peek
                val BroadcastObject(stack, req) = bObj

                val real = invProvider.getInventory
                if (real == null)
                {
                    popAll()
                    req.itemLost(stack)
                    cont.break()
                }

                if (!routeLayer.getRouter.canRouteTo(req.getRouter.getIPAddress, stack.key, bObj.priority))
                {
                    popAll()
                    req.itemLost(stack)
                    cont.break()
                }

                var toExtract = stack.stackSize
                toExtract = math.min(toExtract, itemsRemaining)
                toExtract = math.min(toExtract, stack.key.getMaxStackSize)

                var restack = false

                val dspace = req.getActiveFreeSpace(stack.key)
                if (dspace < toExtract)
                {
                    toExtract = dspace
                    if (toExtract <= 0)
                    {
                        restackOrders()
                        wh.break()
                    }
                    restack = true
                }

                val removed = extractItem(stack.key, toExtract)
                if (removed <= 0 && timeOutOnFailedExtract)
                {
                    popAll()
                    req.itemLost(stack)
                    cont.break()
                }

                if (removed > 0)
                {
                    val toSend = stack.key.makeStack(removed)
                    routeLayer.queueStackToSend(toSend, invProvider.getInterfacedSide, bObj.priority, req.getRouter.getIPAddress)
                }

                if (!pop(removed) && restack) restackOrders()

                stacksRemaining -= 1
                itemsRemaining -= removed
            }
        }
    }
}

class ChipBroadcaster extends RoutingChip with TChipFilter with TChipOrientation with TChipPriority with TActiveBroadcastStack
{
    filterExclude = true

    private var timeRemaining = operationDelay

    def operationDelay = 10

    override def getStacksToExtract = 8

    override def getItemsToExtract = 64

    override def timeOutOnFailedExtract = true

    override def extractItem(item:ItemKey, amount:Int) =
    {
        val real = invProvider.getInventory
        if (real != null)
        {
            val inv = applyFilter(InvWrapper.wrap(real)).setSlotsFromSide(side)
            inv.extractItem(item, amount)
        }
        else 0
    }

    override def update()
    {
        timeRemaining -= 1
        if (timeRemaining > 0) return
        timeRemaining = operationDelay

        doExtractOperation()
    }

    override def requestPromise(request:RequestBranchNode, existingPromises:Int)
    {
        val real = invProvider.getInventory
        if (real == null) return

        val inv = applyFilter(InvWrapper.wrap(real)).setSlotsFromSide(side)
        val filt = applyFilter(InvWrapper.wrap(filter), hide=false)

        val requested = request.getRequestedPackage

        if (filt.hasItem(requested) != filterExclude)
        {
            var numberAvailable = inv.getItemCount(requested)
            numberAvailable -= existingPromises
            if (numberAvailable > 0) request.addPromise(
                new DeliveryPromise(requested, math.min(
                    request.getMissingCount, numberAvailable), routeLayer.getBroadcaster)
            )
        }
    }

    override def deliverPromise(promise:DeliveryPromise, requester:IWorldRequester)
    {
        addOrder(ItemKeyStack.get(promise.item, promise.size), requester, Priorities.ACTIVEB)
    }

    override def getBroadcasts(col:ItemQueue)
    {
        val real = invProvider.getInventory
        if (real == null) return

        val inv = applyFilter(InvWrapper.wrap(real)).setSlotsFromSide(side)
        val filt = applyFilter(InvWrapper.wrap(filter), hide=false)

        val items = inv.getAllItemStacks
        for ((k, v) <- items) if (filt.hasItem(k) != filterExclude)
        {
            val toAdd = v-getDeliveryCount(k)
            if (toAdd > 0) col += k -> toAdd
        }
    }

    override def getBroadcastPriority = preference

    override def onRemoved()
    {
        while (hasOrders)
        {
            val BroadcastObject(s, r) = popAll()
            r.itemLost(s)
        }
    }

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addPriorityInfo(list)
        addOrientInfo(list)
        addFilterInfo(list)
    }

    override def enableHiding = true
    override def enableFilter = true
    override def enablePatterns = false

    def getChipType:ChipVal = RoutingChipDefs.ITEMBROADCASTER
}