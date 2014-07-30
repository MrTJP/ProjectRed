package mrtjp.projectred.transportation

import mrtjp.projectred.core.lib.LabelBreaks._
import mrtjp.projectred.core.lib.Pair2
import mrtjp.projectred.core.libmc.inventory.InvWrapper
import mrtjp.projectred.core.libmc.{ItemQueue, ItemKey, ItemKeyStack}
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal

import scala.collection.immutable.HashMap
import scala.collection.mutable.{ListBuffer, Builder => MBuilder}

class ChipBroadcaster extends RoutingChipset with TChipFilter with TChipOrientation with TChipPriority
{
    filterExclude = true

    private var timeRemaining = operationDelay
    private val manager = new DeliveryManager

    def prefScale = 32 //TODO utilize upgrades?

    def stacksToExtract = 1+upgradeBus.LLatency

    def itemsToExtract = 8+upgradeBus.RLatency

    def operationDelay = 5

    def powerPerOp = 1.0D

    override def update()
    {
        timeRemaining -= 1
        if (timeRemaining > 0) return
        timeRemaining = operationDelay

        if (!manager.hasOrders) return

        var next:Pair2[ItemKeyStack, IWorldRequester] = null
        var stacksRemaining = stacksToExtract
        var itemsRemaining = itemsToExtract

        def assign(n:Pair2[ItemKeyStack, IWorldRequester]) = {next=n; next}

        label("while")
        {
            while (manager.hasOrders && assign(manager.peek)!=null && stacksRemaining>0 && itemsRemaining>0) label("cont")
            {
                val real = invProvider.getInventory
                if (real == null)
                {
                    manager.dispatchFailed()
                    break("cont")
                }

                val reqKeyStack = next.get1
                val requester = next.get2

                val inv = applyFilter(InvWrapper.wrap(real)).setSlotsFromSide(side)

                if (!routeLayer.getRouter.canRouteTo(requester.getRouter.getIPAddress, reqKeyStack.key, SendPriority.ACTIVEB))
                {
                    manager.dispatchFailed()
                    break("cont")
                }

                var toExtract = Math.min(inv.getItemCount(reqKeyStack.key), reqKeyStack.stackSize)
                toExtract = Math.min(toExtract, itemsRemaining)
                toExtract = Math.min(toExtract, reqKeyStack.makeStack.getMaxStackSize)

                var restack = false

                val destinationSpace = requester.getActiveFreeSpace(reqKeyStack.key)
                if (destinationSpace < toExtract)
                {
                    toExtract = destinationSpace
                    if (toExtract <= 0)
                    {
                        manager.restackOrders()
                        break("while")
                    }
                    restack = true
                }

                if (!controller.canUsePower(toExtract*powerPerOp)) break("while")

                val removed = inv.extractItem(reqKeyStack.key, toExtract)
                if (removed <= 0)
                {
                    manager.dispatchFailed()
                    break("cont")
                }

                controller.usePower(removed*powerPerOp)

                val toSend = reqKeyStack.key.makeStack(removed)
                routeLayer.queueStackToSend(toSend, invProvider.getInterfacedSide, SendPriority.ACTIVEB, requester.getRouter.getIPAddress)
                manager.dispatchSuccessful(removed, restack)

                stacksRemaining -= 1
                itemsRemaining -= removed
            }
        }
    }

    override def requestPromises(request:RequestBranchNode, existingPromises:Int)
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
                new DeliveryPromise(requested, Math.min(
                    request.getMissingCount, numberAvailable), routeLayer.getBroadcaster)
            )
        }
    }

    override def deliverPromises(promise:DeliveryPromise, requester:IWorldRequester)
    {
        manager.addOrder(ItemKeyStack.get(promise.item, promise.size), requester)
    }

    override def getProvidedItems(col:ItemQueue)
    {
        val real = invProvider.getInventory
        if (real == null) return

        val inv = applyFilter(InvWrapper.wrap(real)).setSlotsFromSide(side)
        val filt = applyFilter(InvWrapper.wrap(filter), hide=false)

        val items = inv.getAllItemStacks
        for ((k, v) <- items) if (filt.hasItem(k) != filterExclude)
        {
            val toAdd = v-manager.getDeliveryCount(k)
            if (toAdd > 0) col += k -> toAdd
        }
    }

    override def getBroadcastPriority = preference

    override def onPipeBroken()
    {
        while (manager.hasOrders) manager.dispatchFailed()
    }

    override def createUpgradeBus =
    {
        val bus = new UpgradeBus(3, 3)
        bus.setLatency(1, 2, 4, 8, 16, 32)
        bus.Linfo = "stacks to check in one operation"
        bus.Lformula = "stacks = 1 + Latency"
        bus.Rinfo = "items to extract in one operation"
        bus.Rformula = "items = 8 + Latency"

        bus
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
