package mrtjp.projectred.transportation

import mrtjp.projectred.core.libmc.{ItemKey, ItemKeyStack}

/**
 * Copyright (c) 2014 MrTJP inc.
 * All rights reserved.
 */

class RequestMain(destination:IWorldRequester, opt:RequestFlags.ValueSet)
{
    private var operations = Vector[RequestOperation]()

    //pointer
    var operation:RequestOperation = null
    var action:SupplyAction = null

    //interactions
    def promised = operation.promised
    def missing = operation.missing
    def extra = operation.extra

    def addAction(a:SupplyAction)
    {
        action = a
        operation.register(action)
    }

    def addDeps(ops:RequestOperation*)
    {
        ops.foreach(operation.register)
    }

    def start()
    {
        for (op <- operations)
        {
            operation = op

            val allRouters = destination.getRouter
                .getFilteredRoutesByCost(p => p.flagRouteFrom && p.allowBroadcast && p.allowItem(op.item.key))
                .sorted(PathOrdering.metric)
        }
    }
}

class RequestOperation(val to:IWorldRequester, val item:ItemKeyStack)
{
    var dependencies = Vector[RequestOperation]()
    var actions = Vector[SupplyAction]()

    var promised = 0
    var missing = item.stackSize
    var extra = 0

    def finished = promised >= item.stackSize

    private def count()
    {
        promised = actions.foldLeft(0)((c, a) => c+a.size)
        missing = if (item.stackSize > promised) item.stackSize-promised else 0
        extra = if (promised > item.stackSize) promised-item.stackSize else 0
    }

    def register(action:SupplyAction)
    {
        actions :+= action
        action.to = to
        count()
    }

    def register(dep:RequestOperation)
    {
        dependencies :+= dep
    }
}

class SupplyAction(val from:IWorldBroadcaster, val item:ItemKey, var size:Int, var isExcess:Boolean = false, var used:Boolean = false)
{
    var to:IWorldRequester = _

    def startAction() = from match
    {
        case wc:IWorldCrafter if isExcess => wc.actOnExcess(this)
        case _ => from.act(this)
    }

    def copy = new SupplyAction(from, item, size, isExcess, used)

    def split(at:Int) =
    {
        val action = new SupplyAction(from, item, size-at, isExcess, used)
        size = at
        action
    }
}

class CraftAction(val from:IWorldCrafter, val item:ItemKey, size:Int, priority:Int)
{
}