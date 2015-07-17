package mrtjp.projectred.transportation

import java.util.{PriorityQueue => JPriorityQueue}

import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.core.util.{HashPair2, Pair2}
import net.minecraft.item.ItemStack

import scala.collection.immutable.{HashMap, TreeSet}
import scala.collection.mutable.{HashMap => MHashMap}

object RequestFlags extends Enumeration
{
    type RequestFlags = Value
    val PULL, CRAFT, PARTIAL, SIMULATE = Value

    def all = PULL+CRAFT+PARTIAL+SIMULATE
    def full = PULL+CRAFT+PARTIAL
    def default = PULL+CRAFT
}

class RequestBranchNode(parentCrafter:CraftingPromise, thePackage:ItemKeyStack, requester:IWorldRequester, parent:RequestBranchNode, opt:RequestFlags.ValueSet)
{
    val root:RequestRoot =
    {
        if (parent != null)
        {
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
    def getMissingCount = thePackage.stackSize-promisedCount
    def getRequestedPackage = thePackage.key
    def isDone = getMissingCount <= 0

    def addPromise(promise:DeliveryPromise)
    {
        assert(promise.item == getRequestedPackage)

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
            .getFilteredRoutesByCost(p => p.flagRouteFrom && p.allowBroadcast && p.allowItem(getRequestedPackage))
            .sorted(PathOrdering.metric)
        def search()
        {
            for (l <- allRouters) if (isDone) return else l.end.getParent match
            {
                case member:IWorldBroadcaster =>
                    if (!LogisticPathFinder.sharesInventory(requester.getContainer, member.getContainer))
                    {
                        val prev = root.getExistingPromisesFor(member, getRequestedPackage)
                        member.requestPromise(this, prev)
                    }
                case _ =>
            }
        }
        search()
        isDone
    }

    def doExcessReq() =
    {
        val all = root.gatherExcessFor(getRequestedPackage)
        def locate()
        {
            import scala.util.control.Breaks._
            for (excess <- all) if (isDone) return else if (excess.size > 0) breakable
            {
                val pathsToThis = requester.getRouter.getRouteTable(excess.from.getRouter.getIPAddress)
                val pathsFromThat = excess.from.getRouter.getRouteTable(requester.getRouter.getIPAddress)
                for (from <- pathsFromThat) if (from != null && from.flagRouteTo)
                    for (to <- pathsToThis) if (to != null && to.flagRouteFrom)
                    {
                        excess.size = Math.min(excess.size, getMissingCount)
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
            .getFilteredRoutesByCost(p => p.flagRouteFrom && p.allowCrafting && p.allowItem(getRequestedPackage))
            .sorted(PathOrdering.load)

        var jobs = Vector.newBuilder[CraftingPromise]
        for (l <- allRouters) l.end.getParent match
        {
            case wc:IWorldCrafter =>
                val item = recurse_GetCrafterItem(wc)
                if (item == null || item == getRequestedPackage) //dont use a crafter that has been used for a different item in this request tree
                {
                    val cpl = wc.requestCraftPromise(getRequestedPackage)
                    for (cp <- cpl) jobs += cp
                }
            case _ =>
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
    protected def recurse_GetCrafterItem(crafter:IWorldCrafter):ItemKey =
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
    protected def recurse_GatherExcess(item:ItemKey, excessMap:MHashMap[IWorldBroadcaster, Vector[DeliveryPromise]])
    {
        for (excess <- excessPromises) if (excess.item == item)
        {
            var prev = excessMap.getOrElse(excess.from, Vector[DeliveryPromise]())
            prev :+= excess.copy
            excessMap += excess.from -> prev
        }
        for (subNode <- subRequests) subNode.recurse_GatherExcess(item, excessMap)
    }
    protected def recurse_RemoveUnusableExcess(item:ItemKey, excessMap:MHashMap[IWorldBroadcaster, Vector[DeliveryPromise]])
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
        for (p <- excessPromises) p.from match
        {
            case wc:IWorldCrafter => wc.registerExcess(p)
            case _ =>
        }
    }

    def recurse_RebuildParityTree()
    {
        if (isDone) return
        if (parityBranch == null) return

        val setsNeeded = (getMissingCount+parityBranch.getSizeForSet-1)/parityBranch.getSizeForSet
        val components = parityBranch.getScaledIngredients(setsNeeded)

        for (pair <- components) new RequestBranchNode(parityBranch, pair.get1, pair.get2, this, RequestFlags.default)

        addPromise(parityBranch.getScaledPromise(setsNeeded))
        subRequests.foreach(_.recurse_RebuildParityTree())
    }

    def recurse_GatherStatisticsMissing(map:MHashMap[ItemKey, Int])
    {
        val missing = getMissingCount
        if (missing > 0)
        {
            val item = getRequestedPackage
            val count = map.getOrElse(item, 0)+missing
            map += item -> count
        }
        subRequests.foreach(_.recurse_GatherStatisticsMissing(map))
    }

    def recurse_GatherStatisticsUsed(map:MHashMap[ItemKey, Int])
    {
        var thisUsed = 0
        for (p <- promises) if (!p.from.isInstanceOf[IWorldCrafter]) thisUsed += p.size
        if (thisUsed > 0)
        {
            val item = getRequestedPackage
            val count = map.getOrElse(item, 0)+thisUsed
            map += item -> count
        }
        subRequests.foreach(_.recurse_GatherStatisticsUsed(map))
    }
}

class RequestRoot(thePackage:ItemKeyStack, requester:IWorldRequester, opt:RequestFlags.ValueSet) extends RequestBranchNode(null, thePackage, requester, null, opt)
{
    var tableOfPromises = new HashMap[HashPair2[IWorldBroadcaster, ItemKey], Int]()

    def getExistingPromisesFor(b:IWorldBroadcaster, item:ItemKey):Int = getExistingPromisesFor(new HashPair2(b, item))
    def getExistingPromisesFor(pair:HashPair2[IWorldBroadcaster, ItemKey]) =
    {
        //This may not be constucted yet, because this is called from code before the constructor
        if (tableOfPromises == null) tableOfPromises = new HashMap[HashPair2[IWorldBroadcaster, ItemKey], Int]()
        tableOfPromises.getOrElse(pair, 0)
    }

    def promiseAdded(p:DeliveryPromise)
    {
        val key = new HashPair2(p.from, p.item)
        tableOfPromises += key -> (getExistingPromisesFor(key)+p.size)
    }

    def gatherExcessFor(item:ItemKey) =
    {
        val excessMap = new MHashMap[IWorldBroadcaster, Vector[DeliveryPromise]]

        recurse_GatherExcess(item, excessMap)
        recurse_RemoveUnusableExcess(item, excessMap)

        var all = Vector.newBuilder[DeliveryPromise]
        excessMap.foreach(all ++= _._2)
        all.result()
    }

    def promiseRemoved(promise:DeliveryPromise)
    {
        val key = new HashPair2(promise.from, promise.item)
        val newCount = getExistingPromisesFor(key)-promise.size
        if (newCount <= 0) tableOfPromises = tableOfPromises.filterNot(_._1 == key)
        else tableOfPromises += key -> newCount
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

        def wr1 = x.end.getParent
        def wr2 = y.end.getParent

        val p1 = wr1 match
        {
            case b:IWorldBroadcaster => b.getBroadcastPriority
            case _ => Integer.MIN_VALUE
        }
        val p2 = wr2 match
        {
            case b:IWorldBroadcaster => b.getBroadcastPriority
            case _ => Integer.MIN_VALUE
        }

        if (p1 != p2) return if (p2 > p1) 1 else -1

        var switchKey = 1
        if (x.end.getIPAddress-y.end.getIPAddress > 0)
        {
            switchKey = -1
            val temp = x
            x = y
            y = temp
        }

        val l1 = wr1 match
        {
            case b:IWorldBroadcaster => b.getWorkLoad
            case _ => 0.0D
        }
        val l2 = wr2 match
        {
            case b:IWorldBroadcaster => b.getWorkLoad
            case _ => 0.0D
        }

        c = l1-l2
        c += (x.distance-y.distance)*distanceWeight

        if (c == 0) -switchKey //if same distance lower id is preferred
        else if (c > 0) (c+0.5D).toInt*switchKey //round up
        else (c-0.5D).toInt*switchKey //round down
    }
}

class DeliveryPromise(var item:ItemKey, var size:Int, var from:IWorldBroadcaster, var isExcess:Boolean = false, var used:Boolean = false)
{
    def copy = new DeliveryPromise(item, size, from, isExcess, used)
}

class CraftingPromise(val result:ItemKeyStack, val crafter:IWorldCrafter, val priority:Int) extends Ordered[CraftingPromise]
{
    var ingredients = Vector[Pair2[ItemKeyStack, IWorldRequester]]()

    def addIngredient(ingredient:ItemKeyStack, crafter:IWorldRequester):CraftingPromise =
    {
        for (i <- ingredients) if ((i.get1.key == ingredient.key) && i.get2 == crafter)
        {
            i.get1.stackSize += ingredient.stackSize
            return this
        }
        ingredients :+= new Pair2(ingredient, crafter)
        this
    }

    def getScaledPromise(sets:Int) = new DeliveryPromise(result.key.copy, result.stackSize*sets, crafter)

    def getScaledIngredients(sets:Int) =
    {
        var components = Vector.newBuilder[Pair2[ItemKeyStack, IWorldRequester]]
        for (i <- ingredients)
        {
            val newI = new Pair2(i.get1.copy, i.get2)
            newI.get1.stackSize *= sets
            components += newI
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
        val setsToCraft = Math.min(setsRequested, maxSetsAvailable)
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
        var subs = Vector.newBuilder[RequestBranchNode]
        val ingredients = crafter.getScaledIngredients(numberOfSets)

        for (item <- ingredients)
        {
            val req = new RequestBranchNode(crafter, item.get1, item.get2, branch, RequestFlags.default)
            subs += req
            if (!req.isDone) failed = true
        }

        if (failed)
        {
            val children = subs.result()
            children.foreach(_.destroy())

            branch.parityBranch = crafter
            for (i <- 0 until ingredients.size)
                potentialSets = Math.min(potentialSets,
                    children(i).getPromisedCount/(ingredients(i).get1.stackSize/numberOfSets))

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

            for (item <- ingredients)
            {
                val req = new RequestBranchNode(crafter, item.get1, item.get2, branch, RequestFlags.default)
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
    private var destination:IWorldRequester = null
    private var branch:RequestRoot = null
    private var used:Map[ItemKey, Int] = null
    private var missing:Map[ItemKey, Int] = null

    var requested = 0

    def setDestination(destination:IWorldRequester) =
    {
        this.destination = destination
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
        used = null
        missing = null
        requested = 0
        branch = new RequestRoot(request.copy, destination, opt)

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

    private def gatherUsed()
    {
        if (used == null)
        {
            rebuildParity()
            val u = MHashMap[ItemKey, Int]()
            branch.recurse_GatherStatisticsUsed(u)
            used = u.toMap
        }
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

    def getUsed =
    {
        if (used == null) gatherUsed()
        used
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