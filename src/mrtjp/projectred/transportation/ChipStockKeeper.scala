package mrtjp.projectred.transportation

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import mrtjp.projectred.core.libmc.{ItemKeyStack, ItemKey}
import mrtjp.projectred.core.libmc.inventory.InvWrapper

class ChipStockKeeper extends RoutingChipset with TChipStock
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

    def powerPerOp = 10.0D

    var enrouteItems = HashMap[ItemKey, Int]().withDefaultValue(0)

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
            if (!controller.canUsePower(powerPerOp)) break()

            val req = new RequestConsole(RequestFlags.full).setDestination(routeLayer.getRequester)
            val request = ItemKeyStack.get(keyStack.key, missing)
            req.makeRequest(request)

            requestAttempted = true

            if (req.requested > 0)
            {
                addToRequestList(request.key, req.requested)
                requestedSomething = true
                controller.usePower(powerPerOp)
            }
        }

        if (requestAttempted) RouteFX.spawnType1(RouteFX.color_request, 8, routeLayer.getCoords, routeLayer.getWorld)
        if (requestAttempted && requestedSomething) operationsWithoutRequest = 0
        else operationsWithoutRequest += 1

        remainingDelay = operationDelay+throttleDelay
    }

    def addToRequestList(item:ItemKey, amount:Int)
    {
        enrouteItems += item -> (enrouteItems(item)+amount)
    }

    def removeFromRequestList(item:ItemKey, amount:Int)
    {
        var current = enrouteItems(item)
        if (current != 0)
        {
            current-=amount
            if (current <= 0) enrouteItems -= item
            else enrouteItems += item -> current
        }
    }

    def getEnroute(item:ItemKey) = enrouteItems(item)

    override def trackedItemLost(s:ItemKeyStack)
    {
        removeFromRequestList(s.key, s.stackSize)
    }

    override def trackedItemReceived(s:ItemKeyStack)
    {
        removeFromRequestList(s.key, s.stackSize)
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
