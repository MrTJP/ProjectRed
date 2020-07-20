package mrtjp.projectred.transportation

import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemKey

import scala.collection.mutable.ListBuffer

class ChipItemResponder extends RoutingChip with TChipFilter with TChipPriority
{
    def sendPriority = Priorities.PASSIVE

    override def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse =
    {
        val real = invProvider.getInventory
        val side = invProvider.getInterfacedSide

        if (real == null || side < 0) return null

        if (SyncResponse.isPreferredOver(sendPriority.ordinal, preference, rival) && filterAllows(item))
        {
            val inv = InvWrapper.wrap(real).setSlotsFromSide(side)
            val room = inv.getSpaceForItem(item)
            if (room > 0) return new SyncResponse().setPriority(sendPriority).setCustomPriority(preference).setItemCount(room)
        }

        null
    }

    def filterAllows(item:ItemKey) = !enableFilter || applyFilter(InvWrapper.wrap(filter)).hasItem(item) != filterExclude

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addPriorityInfo(list)
        addFilterInfo(list)
    }

    def getChipType = RoutingChipDefs.ITEMRESPONDER

    override def enableHiding = false
}

class ChipItemOverflowResponder extends ChipItemResponder
{
    override def sendPriority = Priorities.DEFAULT

    override def getChipType = RoutingChipDefs.ITEMOVERFLOWRESPONDER

    override def enableFilter = false
    override def enablePatterns = false
}

class ChipItemTerminator extends ChipItemResponder
{
    override def sendPriority = Priorities.TERMINATED

    override def getChipType = RoutingChipDefs.ITEMTERMINATOR
}

class ChipDynamicItemResponder extends ChipItemResponder
{
    override def getChipType = RoutingChipDefs.DYNAMICITEMRESPONDER

    override def enableFilter = false

    override def filterAllows(item:ItemKey) = applyFilter(InvWrapper.wrap(invProvider.getInventory)).hasItem(item)
}
