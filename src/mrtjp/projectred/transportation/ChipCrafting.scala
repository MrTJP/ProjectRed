package mrtjp.projectred.transportation

import scala.collection.mutable.ListBuffer

class ChipCrafting extends RoutingChipset with TChipCrafter with TChipPriority
{
    def maxExtensions = upgradeBus.RLatency

    def prefScale = upgradeBus.LLatency

    override def enablePriorityFlag = true

    override def createUpgradeBus =
    {
        val bus = new UpgradeBus(3, 3)
        bus.setLatency(2, 6, 8, 1, 3, 5)
        bus.Linfo = "raise max priority value"
        bus.Lformula = "priority value = Latency"
        bus.Rinfo = "number of extensions"
        bus.Rformula = "extensions = Latency"

        bus
    }

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addMatrixInfo(list)
        if (prefScale > 0) addPriorityInfo(list)
        if (maxExtensions > 0) addExtInfo(list)
    }

    def getChipType = RoutingChipDefs.ITEMCRAFTING
}
