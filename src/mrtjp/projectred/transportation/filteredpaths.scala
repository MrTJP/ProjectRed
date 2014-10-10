package mrtjp.projectred.transportation

import mrtjp.projectred.core.libmc.ItemKey


class StartEndPath(var start:Router, var end:Router, var hopDir:Int, var distance:Int, filters:Set[PathFilter]) extends Path(filters) with Ordered[StartEndPath]
{
    def this(start:Router, end:Router, dirToFirstHop:Int, distance:Int, filter:PathFilter) = this(start, end, dirToFirstHop, distance, Set(filter))
    def this(start:Router, end:Router, dirToFirstHop:Int, distance:Int) = this(start, end, dirToFirstHop, distance, PathFilter.default)

    override def equals(other:Any) = other match
    {
        case that:StartEndPath =>
                hopDir == that.hopDir &&
                distance == that.distance &&
                super.equals(that)
        case _ => false
    }

    def -->(to:StartEndPath) = new StartEndPath(start, to.end, hopDir, distance+to.distance, filters ++ to.filters)

    override def compare(that:StartEndPath) =
    {
        var c = distance-that.distance
        if (c == 0) c = end.getIPAddress-that.end.getIPAddress
        c
    }
}

class Path(val filters:Set[PathFilter])
{
    override def equals(other:Any) = other match
    {
        case that:Path =>
            networkFlags == that.networkFlags &&
                pathFlags == that.pathFlags &&
                filters == that.filters
        case _ => false
    }

    val emptyFilter = filters.forall(_ == PathFilter.default)

    val networkFlags = filters.foldLeft(0x7)((b, f) => f.networkFlags&b)
    val allowRouting = (networkFlags&0x1) != 0
    val allowBroadcast = (networkFlags&0x2) != 0
    val allowCrafting = (networkFlags&0x4) != 0

    def allowItem(item:ItemKey):Boolean = filters.forall(_.allowItem(item))
    def allowColor(c:Int) = filters.forall(_.allowColor(c))

    val pathFlags = filters.foldLeft(0x3)((b, f) => f.pathFlags&b)
    val flagRouteTo = (pathFlags&0x1) != 0
    val flagRouteFrom = (pathFlags&0x2) != 0
}

object PathFilter
{
    val default = new PathFilter
}

class PathFilter
{
    /**
     * 0CBR
     * R - allow Routing
     * B - allow Broadcastint
     * C - allow Crafting
     */
    var networkFlags = 0x7

    /**
     * 00FT
     * T - can travel to
     * F - can come from
     */
    var pathFlags = 0x3

    //Filter for items
    var filterExclude = true
    var itemFilter:Set[ItemKey] = Set.empty
    def allowItem(item:ItemKey) = itemFilter.contains(item) != filterExclude

    //Filter for colors
    var colorExclude = true
    var colors = 0
    def filterContainsColor(c:Int) = (colors&1<<c) != 0
    def allowColor(c:Int) = if (0 until 16 contains c)
        filterContainsColor(c) != colorExclude else true

    override def equals(other:Any) = other match
    {
        case that:PathFilter =>
            networkFlags == that.networkFlags &&
                pathFlags == that.pathFlags
                filterExclude == that.filterExclude &&
                itemFilter == that.itemFilter &&
                colorExclude == that.colorExclude &&
                colors == that.colors
        case _ => false
    }
}