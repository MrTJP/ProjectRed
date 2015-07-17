package mrtjp.projectred.transportation

import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.{ItemQueue, ItemKey, ItemKeyStack}

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class ChipStockKeeper extends RoutingChip with TChipStock
{
    private var remainingDelay = operationDelay
    private def operationDelay = 100

    private var operationsWithoutRequest = 0
    private def throttleDelay =
    {
        var throttle = 10*operationsWithoutRequest
        throttle = Math.min(throttle, 20*60)
        throttle
    }

    var enrouteItems = new ItemQueue

    override def update()
    {
        super.update()

        remainingDelay -= 1
        if (remainingDelay > 0) return

        val real = invProvider.getInventory
        val side = invProvider.getInterfacedSide
        if (real == null || side < 0) return

        val inv = InvWrapper.wrap(real).setSlotsFromSide(side)
        val filt = InvWrapper.wrap(stock).setSlotsAll()

        var checked = Set[ItemKey]()
        var requestAttempted = false
        var requestedSomething = false

        import scala.util.control.Breaks._
        for (i <- 0 until stock.getSizeInventory) breakable
        {
            val keyStack = ItemKeyStack.get(stock.getStackInSlot(i))
            if (keyStack == null || checked.contains(keyStack.key)) break()
            checked += keyStack.key

            val toRequest = filt.getItemCount(keyStack.key)
            val inInventory = inv.getItemCount(keyStack.key)+getEnroute(keyStack.key)
            val spaceInInventory = routeLayer.getRequester.getActiveFreeSpace(keyStack.key)
            val missing = math.min(toRequest-inInventory, spaceInInventory)
            if (missing <= 0 || (requestWhenEmpty && inInventory > 0)) break()

            val req = new RequestConsole(RequestFlags.full).setDestination(routeLayer.getRequester)
            val request = ItemKeyStack.get(keyStack.key, missing)
            req.makeRequest(request)

            requestAttempted = true

            if (req.requested > 0)
            {
                addToRequestList(request.key, req.requested)
                requestedSomething = true
            }
        }

        if (requestAttempted) RouteFX2.spawnType1(RouteFX2.color_request, routeLayer.getWorldRouter.getContainer)
        if (requestAttempted && requestedSomething) operationsWithoutRequest = 0
        else operationsWithoutRequest += 1

        remainingDelay = operationDelay+throttleDelay
    }

    def addToRequestList(item:ItemKey, amount:Int)
    {
        enrouteItems += item -> (enrouteItems(item)+amount)
    }

    def getEnroute(item:ItemKey) = enrouteItems(item)


    override def onEventReceived(event:NetworkEvent) = event match
    {
        case e:ItemLostEvent =>
            val toRem = math.min(enrouteItems(e.item), e.remaining)
            enrouteItems.remove(e.item, toRem)
            e.remaining -= toRem
            if (e.remaining <= 0) e.setCanceled()
        case e:ItemReceivedEvent =>
            val toRem = math.min(enrouteItems(e.item), e.remaining)
            enrouteItems.remove(e.item, toRem)
            e.remaining -= toRem
            if (e.remaining <= 0) e.setCanceled()
        case _ =>
    }

    override def weakTileChanges = true

    override def onNeighborTileChanged(side:Int, weak:Boolean)
    {
        operationsWithoutRequest = 0
        remainingDelay = Math.min(remainingDelay, operationDelay)
    }

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addStockInfo(list)
    }

    def getChipType = RoutingChipDefs.ITEMSTOCKKEEPER
}
