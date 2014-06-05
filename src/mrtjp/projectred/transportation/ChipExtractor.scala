package mrtjp.projectred.transportation

import scala.collection.immutable.BitSet
import scala.collection.mutable.ListBuffer
import mrtjp.projectred.core.libmc.inventory.InvWrapper

class ChipExtractor extends RoutingChipset with TChipFilter with TChipOrientation
{
    private var remainingDelay = operationDelay

    private def operationDelay = 100-upgradeBus.LLatency

    private def itemsToExtract = 8+upgradeBus.RLatency

    override def update()
    {
        super.update()

        remainingDelay-=1
        if (remainingDelay>0) return
        remainingDelay = operationDelay

        val real = invProvider.getInventory
        if (real == null) return

        val inv = InvWrapper.wrap(real).setSlotsFromSide(side)
        val filt = applyFilter(InvWrapper.wrap(filter))

        val available = inv.getAllItemStacks
        for ((k,v) <- available)
        {
            val stackKey = k
            val stackSize = v

            if (stackKey!=null && filt.hasItem(stackKey)!=filterExclude)
            {
                var exclusions = BitSet()
                var s = routeLayer.getLogisticPath(stackKey, exclusions, true)
                if (s != null)
                {
                    var leftInRun = itemsToExtract
                    while (s != null)
                    {
                        var toExtract = Math.min(leftInRun, stackSize)
                        toExtract = Math.min(toExtract, stackKey.getMaxStackSize)
                        if (s.itemCount > 0) toExtract = Math.min(toExtract, s.itemCount)

                        if (toExtract <= 0) return

                        val stack2 = stackKey.makeStack(inv.extractItem(stackKey, toExtract))
                        if (stack2.stackSize <= 0) return

                        routeLayer.queueStackToSend(stack2, invProvider.getInterfacedSide, s)

                        leftInRun -= stack2.stackSize
                        if (leftInRun <= 0) return

                        exclusions += s.responder
                        s = routeLayer.getLogisticPath(stackKey, exclusions, true)
                    }
                }
            }
        }
    }

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addOrientInfo(list)
        addFilterInfo(list)
    }

    def getChipType = RoutingChipDefs.ITEMEXTRACTOR


    override def createUpgradeBus =
    {
        val bus = new UpgradeBus(3, 3)
        bus.setLatency(25, 34, 40, 8, 16, 32)
        bus.Linfo = "delay between extractions"
        bus.Lformula = "delay = 100 - Latency"
        bus.Rinfo = "items to extract in one operation"
        bus.Rformula = "items = 8 + Latency"

        bus
    }

    override def enableHiding = false
}
