package mrtjp.projectred.transportation

import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.{ItemEquality, ItemKey, ItemKeyStack}

import scala.collection.mutable.{ListBuffer, Set => MSet}

class ChipStockKeeper extends RoutingChip with TChipStock with TChipMatchMatrix
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

    private val maxRequestSize = 128

    override def getMatchInventory = stock

    override def update()
    {
        super.update()

        remainingDelay -= 1
        if (remainingDelay > 0) return

        val real = invProvider.getInventory
        val side = invProvider.getInterfacedSide
        if (real == null || side < 0) return

        val inv = real
        val filt = InvWrapper.wrapInternal(stock).setSlotsAll()

        val checked = MSet[ItemKey]()
        var requestAttempted = false
        var requestedSomething = false

        import scala.util.control.Breaks._
        for (i <- 0 until stock.getSizeInventory) breakable
        {
            val keyStack = ItemKeyStack.get(stock.getStackInSlot(i))
            if (keyStack.isEmpty || checked.contains(keyStack.key)) break()
            checked += keyStack.key

            val eq = createEqualityFor(i)
            inv.setMatchOptions(eq)

            val stockToKeep = if (requestMode == 2) Int.MaxValue else filt.getItemCount(keyStack.key)
            val inInventory = inv.getItemCount(keyStack.key)+getEnroute(eq, keyStack.key)
            val spaceInInventory = router.getActiveFreeSpace(keyStack.key)
            var toRequest = math.min(stockToKeep-inInventory, spaceInInventory)
            toRequest = math.min(toRequest, maxRequestSize)
            if (toRequest <= 0 || (requestMode == 1 && inInventory > 0)) break()

            val req = new RequestConsole(RequestFlags.full).setDestination(router).setEquality(eq)
            val request = ItemKeyStack.get(keyStack.key, toRequest)
            req.makeRequest(request)

            requestAttempted = true
            if (req.requested > 0) requestedSomething = true
        }

        if (requestAttempted) RouteFX2.spawnType1(RouteFX2.color_request, router.getPipe)
        if (requestAttempted && requestedSomething) operationsWithoutRequest = 0
        else operationsWithoutRequest += 1

        remainingDelay = operationDelay+throttleDelay
    }

    def getEnroute(eq:ItemEquality, item:ItemKey) = router.getPipe
            .transitQueue.count(eq.matches(item, _))

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
