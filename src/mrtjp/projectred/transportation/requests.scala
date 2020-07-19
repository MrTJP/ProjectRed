package mrtjp.projectred.transportation

import java.util.{PriorityQueue => JPriorityQueue}

import mrtjp.core.item.{ItemQueue, ItemEquality, ItemKey, ItemKeyStack}
import mrtjp.core.util.HashPair2
import net.minecraft.item.ItemStack

import scala.collection.immutable.{HashMap, TreeSet}
import scala.collection.mutable.{HashMap => MHashMap, MultiMap => MMultiMap, Set => MSet, Map => MMap}

object RequestFlags extends Enumeration
{
    type RequestFlags = Value
    val PULL, CRAFT, PARTIAL, SIMULATE = Value

    def all = PULL+CRAFT+PARTIAL+SIMULATE
    def full = PULL+CRAFT+PARTIAL
    def default = PULL+CRAFT
}

class RequestBranchNode(parentCrafter:CraftingPromise, stack:ItemKeyStack, equality:ItemEquality, requester:IRouterContainer, parent:RequestBranchNode, opt:RequestFlags.ValueSet)
{
    val root:RequestRoot =
    {
        if (parent != null) {
            parent.subRequests :+= this
            parent.root
        }
        else this.asInstanceOf[RequestRoot]
    }

    var subRequests = Vector[RequestBranchNode]()

    private var promises = Vector[DeliveryPromise]()
    private var excessPromises = Vector[DeliveryPromise]()

    private var usedCrafters = TreeSet[CraftingPromise]()
    var parityBranch:CraftingPromise = null

    private var promisedCount = 0

    if (parentCrafter != null) if (!recurse_IsCrafterUsed(parentCrafter)) usedCrafters += parentCrafter

    {
        def doRequest()
        {
            if (opt.contains(RequestFlags.PULL) && doPullReq()) return
            if (opt.contains(RequestFlags.CRAFT) && doExcessReq()) return
            if (opt.contains(RequestFlags.CRAFT) && doCraftReq()) return
        }
        doRequest()
    }

    def getPromisedCount = promisedCount
    def getMissingCount = stack.stackSize-promisedCount
    def getRequestedPackage = equality(stack.key)
    def isDone = getMissingCount <= 0

    def addPromise(promise:DeliveryPromise)
    {
        assert(getRequestedPackage.matches(promise.item))

        if (promise.size > getMissingCount)
        {
            val more = promise.size-getMissingCount
            promise.size = getMissingCount
            val excess = new DeliveryPromise(promise.item, more, promise.from, true)
            excessPromises :+= excess
        }

        if (promise.size <= 0) return
        promises :+= promise
        promisedCount += promise.size
        root.promiseAdded(promise)
    }

    def doPullReq() =
    {
        val allRouters = requester.getRouter
            .getFilteredRoutesByCost(p => p.flagRouteFrom && p.allowBroadcast && p.allowItem(stack.key))
            .sorted(PathOrdering.metric)
        def search()
        {
            for (l <- allRouters)
                if (isDone) return
                else {
                    val end = l.end.getContainer
                    if (!LogisticPathFinder.sharesInventory(requester.getPipe, end.getPipe)) {
                        val prev = root.getExistingPromisesFor(end, stack.key)
                        end.requestPromise(this, prev)
                    }
                }
        }
        search()
        isDone
    }

    def doExcessReq() =
    {
        val all = root.gatherExcessFor(stack.key)
        def locate()
        {
            import scala.util.control.Breaks._
            for (excess <- all) if (isDone) return else if (excess.size > 0) breakable
            {
                val pathsToThat = requester.getRouter.getRouteTable(excess.from.getRouter.getIPAddress)
                val pathsFromThat = excess.from.getRouter.getRouteTable(requester.getRouter.getIPAddress)
                for (from <- pathsFromThat) if (from != null && from.flagRouteTo)
                    for (to <- pathsToThat) if (to != null && to.flagRouteFrom)
                    {
                        excess.size = math.min(excess.size, getMissingCount)
                        addPromise(excess)
                        break()
                    }
            }
        }
        locate()
        isDone
    }

    def doCraftReq() =
    {
        val allRouters = requester.getRouter
            .getFilteredRoutesByCost(p => p.flagRouteFrom && p.allowCrafting && p.allowItem(stack.key))
            .sorted(PathOrdering.load)

        var jobs = Vector.newBuilder[CraftingPromise]
        for (l <- allRouters) {

            val wc = l.end.getContainer
            val item = recurse_GetCrafterItem(wc)
            if (item == null || item == stack.key) { //dont use a crafter that has been used for a different item in this request tree
                val cpl = wc.requestCraftPromise(this)
                for (cp <- cpl) jobs += cp
            }
        }

        val it = jobs.result().iterator
        val balanced = new JPriorityQueue[CraftingInitializer](16)
        var unbalanced = Vector[CraftingInitializer]()

        var finished = false
        var priority = 0
        var lastCrafter:CraftingPromise = null

        val outer, inner = new scala.util.control.Breaks
        outer.breakable
        {
            while (!finished) inner.breakable
            {
                if (it.hasNext)
                {
                    if (lastCrafter == null) lastCrafter = it.next()
                }
                else if (lastCrafter == null) finished = true

                var itemsNeeded = getMissingCount

                if (lastCrafter != null && (balanced.isEmpty || priority == lastCrafter.priority))
                {
                    priority = lastCrafter.priority
                    val crafter = lastCrafter
                    lastCrafter = null
                    if (recurse_IsCrafterUsed(crafter)) outer.break()

                    val ci = new CraftingInitializer(crafter, itemsNeeded, this)
                    balanced.add(ci)
                    inner.break()
                }

                if (unbalanced.isEmpty && balanced.isEmpty) inner.break()

                if (balanced.size == 1)
                {
                    unbalanced :+= balanced.poll()
                    unbalanced(0).addAdditionalItems(itemsNeeded)
                }
                else
                {
                    if (!balanced.isEmpty) unbalanced :+= balanced.poll
                    while (unbalanced.nonEmpty && itemsNeeded > 0)
                    {
                        while (!balanced.isEmpty && balanced.peek.toDo <= unbalanced(0).toDo)
                            unbalanced :+= balanced.poll

                        var cap = if (!balanced.isEmpty) balanced.peek.toDo else Int.MaxValue

                        val floor = unbalanced(0).toDo
                        cap = math.min(cap, floor+(itemsNeeded+unbalanced.size-1)/unbalanced.size)

                        for (crafter <- unbalanced)
                        {
                            val request = Math.min(itemsNeeded, cap-floor)
                            if (request > 0) itemsNeeded -= crafter.addAdditionalItems(request)
                        }
                    }
                }

                unbalanced = unbalanced.filterNot(c => c.setsRequested > 0 && !c.finalizeInteraction())

                itemsNeeded = getMissingCount
                if (itemsNeeded <= 0) outer.break()
                if (unbalanced.nonEmpty) finished = false
            }
        }

        isDone
    }

    def destroy()
    {
        parent.remove(this)
    }

    protected def remove(subNode:RequestBranchNode)
    {
        subRequests = subRequests.filterNot(_ == subNode)
        subNode.recurse_RemoveSubPromisses()
    }

    protected def recurse_RemoveSubPromisses()
    {
        promises.foreach(root.promiseRemoved)
        subRequests.foreach(_.recurse_RemoveSubPromisses())
    }
    protected def recurse_GetCrafterItem(crafter:IRouterContainer):ItemKey =
    {
        usedCrafters.find(_.crafter == crafter) match
        {
            case Some(c) => c.getResultItem
            case None if parent != null => parent.recurse_GetCrafterItem(crafter)
            case None => null
        }
    }
    protected def recurse_IsCrafterUsed(crafter:CraftingPromise):Boolean =
    {
        if (usedCrafters.contains(crafter)) true
        else parent != null && parent.recurse_IsCrafterUsed(crafter)
    }
    protected def recurse_GatherExcess(item:ItemKey, excessMap:MHashMap[IRouterContainer, Vector[DeliveryPromise]])
    {
        for (excess <- excessPromises) if (excess.item == item)
        {
            var prev = excessMap.getOrElse(excess.from, Vector[DeliveryPromise]())
            prev :+= excess.copy
            excessMap += excess.from -> prev
        }
        for (subNode <- subRequests) subNode.recurse_GatherExcess(item, excessMap)
    }
    protected def recurse_RemoveUnusableExcess(item:ItemKey, excessMap:MHashMap[IRouterContainer, Vector[DeliveryPromise]])
    {
        for (promise <- promises) if (promise.item == item && promise.isInstanceOf[DeliveryPromise])
        {
            val epromise = promise.asInstanceOf[DeliveryPromise]
            if (!epromise.used)
            {
                var usedcount = epromise.size
                var extras = excessMap.getOrElse(epromise.from, null)
                if (extras != null)
                {
                    var toRem = Vector[DeliveryPromise]()
                    def remove()
                    {
                        for (e <- extras)
                        {
                            if (e.size >= usedcount)
                            {
                                e.size -= usedcount
                                return
                            }
                            else
                            {
                                usedcount -= e.size
                                toRem :+= e
                            }
                        }
                    }
                    remove()
                    extras = extras.filterNot(e => toRem.contains(e))
                    excessMap += epromise.from -> extras
                }
            }
        }
        for (subNode <- subRequests) subNode.recurse_RemoveUnusableExcess(item, excessMap)
    }

    def recurse_StartDelivery()
    {
        subRequests.foreach(_.recurse_StartDelivery())
        for (p <- promises) p.from.deliverPromise(p, requester)
        for (p <- excessPromises) p.from.registerExcess(p)
    }

    def recurse_RebuildParityTree()
    {
        if (isDone) return
        if (parityBranch == null) return

        val setsNeeded = (getMissingCount+parityBranch.getSizeForSet-1)/parityBranch.getSizeForSet
        val components = parityBranch.getScaledIngredients(setsNeeded)

        for ((s, eq, d) <- components) new RequestBranchNode(parityBranch, s, eq, d, this, RequestFlags.default)

        addPromise(parityBranch.getScaledPromise(setsNeeded))
        subRequests.foreach(_.recurse_RebuildParityTree())
    }

    def recurse_GatherStatisticsMissing(map:MHashMap[ItemKey, Int])
    {
        val missing = getMissingCount
        if (missing > 0)
        {
            val item = stack.key
            val count = map.getOrElse(item, 0)+missing
            map += item -> count
        }
        subRequests.foreach(_.recurse_GatherStatisticsMissing(map))
    }
}

class RequestRoot(thePackage:ItemKeyStack, equality:ItemEquality, requester:IRouterContainer, opt:RequestFlags.ValueSet) extends RequestBranchNode(null, thePackage, equality:ItemEquality, requester, null, opt)
{
    var tableOfPromises:MMap[IRouterContainer, ItemQueue] = _

    def getExistingPromisesFor(b:IRouterContainer, item:ItemKey) =
    {
        if (tableOfPromises == null) tableOfPromises = MMap[IRouterContainer, ItemQueue]()
        tableOfPromises.get(b) match {
            case Some(queue) => queue(item)
            case _ => 0
        }
    }

    def promiseAdded(p:DeliveryPromise)
    {
        tableOfPromises.getOrElseUpdate(p.from, new ItemQueue).add(p.item, p.size)
    }

    def gatherExcessFor(item:ItemKey) =
    {
        val excessMap = new MHashMap[IRouterContainer, Vector[DeliveryPromise]]

        recurse_GatherExcess(item, excessMap)
        recurse_RemoveUnusableExcess(item, excessMap)

        var all = Vector.newBuilder[DeliveryPromise]
        excessMap.foreach(all ++= _._2)
        all.result()
    }

    def promiseRemoved(p:DeliveryPromise)
    {
        tableOfPromises.getOrElseUpdate(p.from, new ItemQueue).remove(p.item, p.size)
    }
}

object PathOrdering
{
    val metric = new PathOrdering(1.0D)
    val load = new PathOrdering(0.0D)
}

class PathOrdering(distanceWeight:Double) extends Ordering[StartEndPath]
{
    override def compare(x1:StartEndPath, y1:StartEndPath):Int =
    {
        var x = x1
        var y = y1

        var c = 0.0D

        def wr1 = x.end.getContainer
        def wr2 = y.end.getContainer

        val p1 = wr1.getBroadcastPriority
        val p2 = wr2.getBroadcastPriority

        if (p1 != p2) return if (p2 > p1) 1 else -1

        var switchKey = 1
        if (x.end.getIPAddress-y.end.getIPAddress > 0)
        {
            switchKey = -1
            val temp = x
            x = y
            y = temp
        }

        val l1 = wr1.getWorkLoad
        val l2 = wr2.getWorkLoad

        c = l1-l2
        c += (x.distance-y.distance)*distanceWeight

        if (c == 0) -switchKey //if same distance lower id is preferred
        else if (c > 0) (c+0.5D).toInt*switchKey //round up
        else (c-0.5D).toInt*switchKey //round down
    }
}

class DeliveryPromise(var item:ItemKey, var size:Int, var from:IRouterContainer, var isExcess:Boolean = false, var used:Boolean = false)
{
    def copy = new DeliveryPromise(item, size, from, isExcess, used)
}

class CraftingPromise(val result:ItemKeyStack, val crafter:IRouterContainer, val priority:Int) extends Ordered[CraftingPromise]
{
    var ingredients2 = Seq.empty[(ItemKeyStack, ItemEquality, IRouterContainer)]

    def addIngredient(stack:ItemKeyStack, eq:ItemEquality, destination:IRouterContainer)
    {
        for ((s, e, d) <- ingredients2) if (s.key == stack.key && d == destination)
        {
            s.stackSize += stack.stackSize
            return
        }
        ingredients2 :+= ((stack, eq, destination))
    }

    def getScaledPromise(sets:Int) = new DeliveryPromise(result.key.copy, result.stackSize*sets, crafter)

    def getScaledIngredients(sets:Int) =
    {
        var components = Seq.newBuilder[(ItemKeyStack, ItemEquality, IRouterContainer)]
        for ((stack, eq, dest) <- ingredients2)
        {
            val copy = stack.copy
            copy.stackSize *= sets
            components += ((copy, eq, dest))
        }
        components.result()
    }

    override def compare(that:CraftingPromise) =
    {
        var c = priority-that.priority
        if (c == 0) c = result.compare(that.result)
        if (c == 0) c = crafter.getRouter.compare(that.crafter.getRouter)
        c
    }

    def getSizeForSet = result.stackSize

    def getResultItem = result.key
}

class CraftingInitializer(crafter:CraftingPromise, maxToCraft:Int, branch:RequestBranchNode) extends Ordered[CraftingInitializer]
{
    val setSize = crafter.getSizeForSet
    val maxSetsAvailable = (branch.getMissingCount+setSize-1)/setSize
    val originalToDo = crafter.crafter.itemsToProcess

    var setsRequested = 0

    def toDo = originalToDo+setsRequested*setSize

    private def calculateMaxPotentialSets(maxSets:Int):Int =
    {
        var needed = 0
        if (maxSets > 0) needed = maxSets
        else needed = (branch.getMissingCount+setSize-1)/setSize
        if (needed <= 0) return 0
        getPotentialSubPromises(needed, crafter)
    }

    def addAdditionalItems(additional:Int):Int =
    {
        val stacksRequested = (additional+setSize-1)/setSize
        setsRequested += stacksRequested
        stacksRequested*setSize
    }

    def finalizeInteraction():Boolean =
    {
        val setsToCraft = math.min(setsRequested, maxSetsAvailable)
        val setsAbleToCraft = calculateMaxPotentialSets(setsToCraft)
        if (setsAbleToCraft > 0)
        {
            val delivery = crafter.getScaledPromise(setsAbleToCraft)
            if (delivery.size != setsAbleToCraft*setSize) return false
            branch.addPromise(delivery)
        }
        val isDone = setsToCraft == setsAbleToCraft
        setsRequested = 0
        isDone
    }

    override def compare(that:CraftingInitializer) = toDo-that.toDo

    def getPotentialSubPromises(numberOfSets:Int, crafter:CraftingPromise) =
    {
        var failed = false
        var potentialSets = numberOfSets
        var subs = Seq.newBuilder[RequestBranchNode]
        val ingredients = crafter.getScaledIngredients(numberOfSets)

        for ((stack, eq, dest) <- ingredients)
        {
            val req = new RequestBranchNode(crafter, stack, eq, dest, branch, RequestFlags.default)
            subs += req
            if (!req.isDone) failed = true
        }

        if (failed)
        {
            val children = subs.result()
            children.foreach(_.destroy())

            branch.parityBranch = crafter
            for (i <- ingredients.indices)
                potentialSets = math.min(potentialSets,
                    children(i).getPromisedCount/(ingredients(i)._1.stackSize/numberOfSets))

            getAbsoluteSubPromises(potentialSets, crafter)
        }
        else potentialSets
    }

    def getAbsoluteSubPromises(numberOfSets:Int, crafter:CraftingPromise):Int =
    {
        var children = Vector.newBuilder[RequestBranchNode]
        if (numberOfSets > 0)
        {
            val ingredients = crafter.getScaledIngredients(numberOfSets)
            var failed = false

            for ((stack, eq, dest) <- ingredients)
            {
                val req = new RequestBranchNode(crafter, stack, eq, dest, branch, RequestFlags.default)
                children += req
                if (!req.isDone) failed = true
            }

            if (failed)
            {
                children.result().foreach(_.destroy())
                return 0
            }
        }
        numberOfSets
    }
}

class RequestConsole(opt:RequestFlags.ValueSet)
{
    var destination:IRouterContainer = null
    var eq = ItemEquality.standard

    private var branch:RequestRoot = null
    private var missing:Map[ItemKey, Int] = null

    var requested = 0

    def setDestination(destination:IRouterContainer) =
    {
        this.destination = destination
        this
    }

    def setEquality(eq:ItemEquality) =
    {
        this.eq = eq
        this
    }

    def makeRequest(request:ItemStack):RequestConsole = makeRequest(ItemKeyStack.get(request))

    def makeRequest(request:ItemKeyStack):RequestConsole =
    {
        buildRequestTree(request)
        startRequest()
    }

    def buildRequestTree(request:ItemKeyStack):RequestConsole =
    {
        assert(destination != null)
        parityBuilt = false
        missing = null
        requested = 0
        branch = new RequestRoot(request.copy, eq, destination, opt)

        this
    }

    def startRequest():RequestConsole =
    {
        if (branch.isDone || opt.contains(RequestFlags.PARTIAL) && branch.getPromisedCount > 0)
        {
            requested = branch.getPromisedCount
            if (!opt.contains(RequestFlags.SIMULATE)) branch.recurse_StartDelivery()
        }
        this
    }

    private var parityBuilt = false
    private def rebuildParity()
    {
        if (!parityBuilt) branch.recurse_RebuildParityTree()
        parityBuilt = true
    }

    private def gatherMissing()
    {
        if (missing == null)
        {
            rebuildParity()
            val m = new MHashMap[ItemKey, Int]
            branch.recurse_GatherStatisticsMissing(m)
            missing = m.toMap
        }
    }

    def getMissing =
    {
        if (missing == null) gatherMissing()
        missing
    }

    def getRequiredPower =
    {
        def count(p:RequestBranchNode):Int =
            p.subRequests.foldLeft(1)((c, r) => c+count(r))

        count(branch)*10.0D
    }
}