package mrtjp.projectred.transportation

import mrtjp.core.inventory.InvWrapper

import scala.collection.immutable.BitSet
import scala.collection.mutable.ListBuffer

class ChipExtractor extends RoutingChip with TChipFilter with TChipOrientation
{
    private var remainingDelay = operationDelay

    private def operationDelay = 10

    private def itemsToExtract = 64

    override def update()
    {
        super.update()

        remainingDelay -= 1
        if (remainingDelay > 0) return
        remainingDelay = operationDelay

        val real = invProvider.getInventory
        if (real == null) return

        val inv = InvWrapper.wrap(real).setSlotsFromSide(side)
        val filt = applyFilter(InvWrapper.wrap(filter))

        val available = inv.getAllItemStacks
        for ((k,v) <- available) {
            val stackKey = k
            val stackSize = v

            if (stackKey != null && filt.hasItem(stackKey) != filterExclude) {
                var exclusions = BitSet.empty
                var s = router.getLogisticPath(stackKey, exclusions, true)
                if (s != null) {
                    var leftInRun = itemsToExtract
                    while (s != null) {
                        var toExtract = math.min(leftInRun, stackSize)
                        toExtract = math.min(toExtract, stackKey.getMaxStackSize)
                        toExtract = math.min(toExtract, s.itemCount)
                        if (toExtract <= 0) return

                        val extracted = inv.extractItem(stackKey, toExtract)
                        if (extracted <= 0) return

                        router.queueStackToSend(stackKey, extracted, s)

                        leftInRun -= extracted
                        if (leftInRun <= 0) return

                        exclusions += s.responder
                        s = router.getLogisticPath(stackKey, exclusions, true)
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

    override def enableHiding = false
}
