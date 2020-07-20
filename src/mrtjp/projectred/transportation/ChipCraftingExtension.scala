/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import java.util.UUID

import mrtjp.core.item.{ItemKey, ItemKeyStack}

import scala.collection.mutable.{HashMap => MHashMap, MultiMap => MMultiMap, Set => MSet, ListBuffer}

object ChipCraftingExtension
{
    //Router UUID -> Set[Extension UUID]
    var map = new MHashMap[UUID, MSet[UUID]] with MMultiMap[UUID, UUID]

    def registerRouter(router:UUID, ext:UUID)
    {
        map.addBinding(router, ext)
    }

    def removeRouter(router:UUID, ext:UUID)
    {
        map.removeBinding(router, ext)
    }

    def getRoutersForExtension(ext:UUID) =
        map.collect {
            case (uuid, set) if set contains ext => uuid
        }
}

class ChipCraftingExtension extends RoutingChip with TChipCrafterExtension with TActiveLostStack
{
    private var remainingDelay = operationDelay

    private def operationDelay = 40

    override def getMaxRequestAttempts = 8

    override def itemLostUnrecoverable(item:ItemKey, amount:Int){}

    override def update()
    {
        remainingDelay -= 1
        if (remainingDelay <= 0)
        {
            remainingDelay = operationDelay
            requestLostItems()
        }
    }

    override def onEventReceived(event:NetworkEvent) = event match
    {
        case e:PayloadLostEnrouteEvent =>
            addLostItem(ItemKeyStack.get(e.item, e.remaining))
            e.remaining = 0
            e.setCanceled()
        case e:TrackedPayloadCancelledEvent =>
            addLostItem(ItemKeyStack.get(e.item, e.remaining))
            e.remaining = 0
            e.setCanceled()
        case _ =>
    }

    override def onAdded()
    {
        ChipCraftingExtension.registerRouter(router.getRouter.getID, id)
    }

    override def onRemoved()
    {
        ChipCraftingExtension.removeRouter(router.getRouter.getID, id)
    }

    override def infoCollection(list:ListBuffer[String])
    {
        super.infoCollection(list)
        addExtIDInfo(list)
    }

    override def getChipType = RoutingChipDefs.ITEMEXTENSION
}