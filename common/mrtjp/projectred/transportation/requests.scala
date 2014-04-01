package mrtjp.projectred.transportation

import collection.mutable.{HashMap => MHashMap}
import mrtjp.projectred.core.utils._
import net.minecraft.item.ItemStack
import scala.collection.immutable.{HashMap, TreeSet}
import scala.collection.mutable
import scala.util.Sorting
import java.util.{PriorityQueue => JPriorityQueue}

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

    private var subRequests = Vector[RequestBranchNode]()

    private var promises = Vector[DeliveryPromise]()
    private var excessPromises = Vector[ExcessPromise]()

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
        if (promise.thePackage != getRequestedPackage) return

        if (promise.size > getMissingCount)
        {
            val more = promise.size-getMissingCount
            promise.size = getMissingCount
            val excess = new ExcessPromise
            excess.setPackage(promise.thePackage).setSize(more).setSender(promise.sender)
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
            .sorted(PathSorter.defaultSort)
        def search()
        {
            for (l <- allRouters) if (isDone) return else l.end.getParent match
            {
                case member:IWorldBroadcaster =>
                    if (!LogisticPathFinder.sharesInventory(requester.getContainer, member.getContainer))
                    {
                        val prev = root.getExistingPromisesFor(member, getRequestedPackage)
                        member.requestPromises(this, prev)
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
            import LabelBreaks._
            for (excess <- all) if (isDone) return else if (excess.size > 0) label
            {
                val pathsToThis = requester.getRouter.getRouteTable(excess.sender.getRouter.getIPAddress)
                val pathsFromThat = excess.sender.getRouter.getRouteTable(requester.getRouter.getIPAddress)
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
            .sorted(PathSorter.workSort)

        var jobs = Vector.newBuilder[CraftingPromise]
        for (l <- allRouters) l.end.getParent match
        {
            case wc:IWorldCrafter =>
                val item = recurse_GetCrafterItem(wc)
                if (item == null || item == getRequestedPackage) //dont use a crafter that has been used for a different item in this request tree
                {
                    val cpl = wc.buildCraftPromises(getRequestedPackage)
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

        import LabelBreaks._
        label("1")
        {
            while (!finished) label
            {
                if (it.hasNext)
                {
                    if (lastCrafter == null) lastCrafter = it.next()
                }
                else if (lastCrafter == null) finished = true

                var itemsNeeded = getMissingCount

                if (lastCrafter != null && (balanced.isEmpty || priority == lastCrafter.getPriority))
                {
                    priority = lastCrafter.getPriority
                    val crafter = lastCrafter
                    lastCrafter = null
                    if (recurse_IsCrafterUsed(crafter)) break("1")

                    val cti = new CraftingInitializer(crafter, itemsNeeded, this)
                    balanced.add(cti)
                    break()
                }

                if (unbalanced.isEmpty && balanced.isEmpty) break()

                if (balanced.size == 1)
                {
                    unbalanced :+= balanced.poll()
                    unbalanced(0).addAdditionalItems(itemsNeeded)
                }
                else
                {
                    if (!balanced.isEmpty) unbalanced :+= balanced.poll
                    while (!unbalanced.isEmpty && itemsNeeded > 0)
                    {
                        while (!balanced.isEmpty && balanced.peek.toDo <= unbalanced(0).toDo)
                            unbalanced :+= balanced.poll

                        var cap = if (!unbalanced.isEmpty) balanced.peek.toDo else Int.MaxValue

                        val floor = unbalanced(0).toDo
                        cap = Math.min(cap, floor+(itemsNeeded+unbalanced.size-1)/unbalanced.size)

                        for (crafter <- unbalanced)
                        {
                            val request = Math.min(itemsNeeded, cap-floor)
                            if (request > 0) itemsNeeded -= crafter.addAdditionalItems(request)
                        }
                    }
                }

                unbalanced = unbalanced.filterNot(c => c.setsRequested > 0 && !c.finalizeInteraction())

                itemsNeeded = getMissingCount
                if (itemsNeeded <= 0) break("1")
                if (!unbalanced.isEmpty) finished = false
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
        for (promise <- promises) root.promiseRemoved(promise)
        for (subNode <- subRequests) subNode.recurse_RemoveSubPromisses()
    }
    protected def recurse_GetCrafterItem(crafter:IWorldCrafter):ItemKey =
    {
        for (c <- usedCrafters) if (c.getCrafter == crafter) return c.getResultItem
        if (parent != null) parent.recurse_GetCrafterItem(crafter)
        else null
    }
    protected def recurse_IsCrafterUsed(crafter:CraftingPromise):Boolean =
    {
        if (usedCrafters.contains(crafter)) true
        else parent != null && parent.recurse_IsCrafterUsed(crafter)
    }
    protected def recurse_GatherExcess(item:ItemKey, excessMap:MHashMap[IWorldBroadcaster, Vector[ExcessPromise]])
    {
        for (excess <- excessPromises) if (excess.thePackage == item)
        {
            var prev = excessMap.getOrElse(excess.sender, Vector[ExcessPromise]())
            prev :+= excess.copy
            excessMap += excess.sender -> prev
        }
        for (subNode <- subRequests) subNode.recurse_GatherExcess(item, excessMap)
    }
    protected def recurse_RemoveUnusableExcess(item:ItemKey, excessMap:MHashMap[IWorldBroadcaster, Vector[ExcessPromise]])
    {
        for (promise <- promises) if (promise.thePackage == item && promise.isInstanceOf[ExcessPromise])
        {
            val epromise = promise.asInstanceOf[ExcessPromise]
            if (!epromise.used)
            {
                var usedcount = epromise.size
                var extras = excessMap.getOrElse(epromise.sender, null)
                if (extras != null)
                {
                    var toRem = Vector[ExcessPromise]()
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
                    excessMap += epromise.sender -> extras
                }
            }

        }
        for (subNode <- subRequests) subNode.recurse_RemoveUnusableExcess(item, excessMap)
    }

    def recurse_StartDelivery()
    {
        for (subReq <- subRequests) subReq.recurse_StartDelivery()
        for (p <- promises) p.sender.deliverPromises(p, requester)
        for (p <- excessPromises) p.sender match
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

        for (pair <- components) new RequestBranchNode(parityBranch, pair.getValue1, pair.getValue2, this, RequestFlags.default)

        addPromise(parityBranch.getScaledPromise(setsNeeded))
        for (sub <- subRequests) sub.recurse_RebuildParityTree()
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
        for (sub <- subRequests) sub.recurse_GatherStatisticsMissing(map)
    }

    def recurse_GatherStatisticsUsed(map:MHashMap[ItemKey, Int])
    {
        var thisUsed = 0
        for (p <- promises) if (!p.sender.isInstanceOf[IWorldCrafter]) thisUsed += p.size
        if (thisUsed > 0)
        {
            val item = getRequestedPackage
            val count = map.getOrElse(item, 0)+thisUsed
            map += item -> count
        }
        for (sub <- subRequests) sub.recurse_GatherStatisticsUsed(map)
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
        val key = new HashPair2(p.sender, p.thePackage)
        tableOfPromises += key -> (getExistingPromisesFor(key)+p.size)
    }

    def gatherExcessFor(item:ItemKey) =
    {
        val excessMap = new MHashMap[IWorldBroadcaster, Vector[ExcessPromise]]

        recurse_GatherExcess(item, excessMap)
        recurse_RemoveUnusableExcess(item, excessMap)

        var all = Vector.newBuilder[ExcessPromise]
        excessMap.foreach(p => all ++= p._2)
        all.result()
    }

    def promiseRemoved(promise:DeliveryPromise)
    {
        val key = new HashPair2(promise.sender, promise.thePackage)
        val newCount = getExistingPromisesFor(key)-promise.size
        if (newCount <= 0) tableOfPromises = tableOfPromises.filterNot(_._1 == key)
        else tableOfPromises += key -> newCount
    }
}

object PathSorter
{
    val defaultSort = new PathSorter(1.0D)
    val workSort = new PathSorter(0.0D)
}

class PathSorter(distanceWeight:Double) extends Ordering[StartEndPath]
{
    override def compare(x1:StartEndPath, y1:StartEndPath):Int =
    {
        var x = x1
        var y = y1

        var c = 0.0D

        val wr1 = x.end.getParent
        val wr2 = y.end.getParent

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

        if (p1 > Integer.MIN_VALUE)
        {
            if (p2 > Integer.MIN_VALUE) c = p2-p1
            else return -1
        }
        else if (p2 > Integer.MIN_VALUE) return 1
        if (c != 0) return c.asInstanceOf[Int]

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

        if (c == 0) -switchKey
        else if (c > 0) (c+0.5).asInstanceOf[Int]*switchKey
        else (c-0.5).asInstanceOf[Int]*switchKey
    }
}

class DeliveryPromise
{
    var thePackage:ItemKey = null
    var size = 0
    var sender:IWorldBroadcaster = null

    def setPackage(thePackage:ItemKey) =
    {
        this.thePackage = thePackage
        this
    }

    def setSize(size:Int) =
    {
        this.size = size
        this
    }

    def setSender(sender:IWorldBroadcaster) =
    {
        this.sender = sender
        this
    }

    def copy =
    {
        val p = new DeliveryPromise
        p.setPackage(thePackage).setSize(size).setSender(sender)
        p
    }
}

class ExcessPromise extends DeliveryPromise
{
    var used = false

    def setUsed(flag:Boolean) =
    {
        used = flag
        this
    }

    override def copy =
    {
        val p = new ExcessPromise
        p.setPackage(thePackage).setSize(size).setSender(sender)
        p.setUsed(used)
        p
    }
}

class CraftingPromise(val result:ItemKeyStack, val crafter:IWorldCrafter, val priority:Int) extends Ordered[CraftingPromise]
{
    var ingredients = Vector[Pair2[ItemKeyStack, IWorldRequester]]()

    def getCrafter = crafter

    def getPriority = priority

    def addIngredient(ingredient:ItemKeyStack, crafter:IWorldRequester):CraftingPromise =
    {
        for (i <- ingredients) if ((i.getValue1.key == ingredient.key) && i.getValue2 == crafter)
        {
            i.getValue1.setSize(i.getValue1.stackSize+ingredient.stackSize)
            return this
        }
        ingredients :+= new Pair2(ingredient, crafter)
        this
    }

    def getScaledPromise(sets:Int) =
    {
        val p = new DeliveryPromise
        p.setPackage(result.key.copy).setSize(result.stackSize*sets).setSender(crafter)
        p
    }

    def getScaledIngredients(sets:Int) =
    {
        var components = Vector.newBuilder[Pair2[ItemKeyStack, IWorldRequester]]
        for (i <- ingredients)
        {
            val newI = new Pair2(i.getValue1.copy, i.getValue2)
            newI.getValue1.setSize(newI.getValue1.stackSize*sets)
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
    val originalToDo = crafter.getCrafter.itemsToProcess

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
        val stacksRequested:Int = (additional+setSize-1)/setSize
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
            val req = new RequestBranchNode(crafter, item.getValue1, item.getValue2, branch, RequestFlags.default)
            subs += req
            if (!req.isDone) failed = true
        }

        if (failed)
        {
            val children = subs.result()
            for (sub <- children) sub.destroy()

            branch.parityBranch = crafter
            for (i <- 0 until ingredients.size)
                potentialSets = Math.min(potentialSets,
                    children(i).getPromisedCount/(ingredients(i).getValue1.stackSize/numberOfSets))

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
                val req = new RequestBranchNode(crafter, item.getValue1, item.getValue2, branch, RequestFlags.default)
                children += req
                if (!req.isDone) failed = true
            }

            if (failed)
            {
                for (sub <- children.result()) sub.destroy()
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
        if (destination == null) return this
        parityBuilt = false
        used = null
        missing = null
        requested = 0
        branch = new RequestRoot(request.copy, destination, opt)
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
}