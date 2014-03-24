package mrtjp.projectred.transportation

import mrtjp.projectred.core.utils.ItemKey

class RoutedFirewallPipe extends RoutedJunctionPipePart
{
    override def routeFilter(forSide:Int) =
    {
        val s = new PathFilter
        {
            override val filterExclude = true
            override val filterItems = Set[ItemKey]()

            override val allowController = true
            override val allowCrafting = false
            override val allowBroadcast = false
            override val allowRouting = !material
        }
        s
    }
}
