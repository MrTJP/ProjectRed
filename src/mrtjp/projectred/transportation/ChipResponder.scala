package mrtjp.projectred.transportation

import mrtjp.projectred.core.libmc.ItemKey
import mrtjp.projectred.core.libmc.inventory.InvWrapper

import scala.collection.mutable.ListBuffer

class ChipItemResponder extends RoutingChipset with TChipFilter with TChipPriority
{
    def sendPriority = SendPriority.PASSIVE

    def prefScale = 2+upgradeBus.LLatency

    def powerPerOp = 1.0D

    override def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse =
    {
        val real = invProvider.getInventory
        val side = invProvider.getInterfacedSide

        if (real == null || side < 0) return null

        if (SyncResponse.isPreferredOver(sendPriority.ordinal, preference, rival) && filterAllows(item) && controller.usePower(powerPerOp))
        {
            val inv = InvWrapper.wrap(real).setSlotsFromSide(side)
            val room = inv.getSpaceForItem(item)
            if (room > 0) return new SyncResponse().setPriority(sendPriority).setCustomPriority(preference).setItemCount(room)
        }

        null
    }

    def filterAllows(item:ItemKey) = applyFilter(InvWrapper.wrap(filter)).hasItem(item) != filterExclude

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addPriorityInfo(list)
        addFilterInfo(list)
    }

    def getChipType = RoutingChipDefs.ITEMRESPONDER

    override def createUpgradeBus =
    {
        val bus = new UpgradeBus(3, 0)
        bus.setLatency(3, 5, 54, 0, 0, 0)
        bus.Linfo = "raise maximum preference value"
        bus.Lformula = "preference value = 2 + Latency"
        bus
    }

    override def enableHiding = false
}

class ChipItemOverflowResponder extends ChipItemResponder
{
    override def sendPriority = SendPriority.DEFAULT

    override def getChipType = RoutingChipDefs.ITEMOVERFLOWRESPONDER

    override def enableFilter = false
    override def enablePatterns = false
}

class ChipItemTerminator extends ChipItemResponder
{
    override def sendPriority = SendPriority.TERMINATED

    override def getChipType = RoutingChipDefs.ITEMTERMINATOR
}

class ChipDynamicItemResponder extends ChipItemResponder
{
    override def getChipType = RoutingChipDefs.DYNAMICITEMRESPONDER

    override def enableFilter = false

    override def filterAllows(item:ItemKey) = applyFilter(InvWrapper.wrap(invProvider.getInventory)).hasItem(item)
}
