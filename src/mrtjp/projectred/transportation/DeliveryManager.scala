package mrtjp.projectred.transportation

import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.core.util.Pair2

class DeliveryManager
{
    private var orders = Vector[Pair2[ItemKeyStack, IWorldRequester]]()

    def addOrder(stack:ItemKeyStack, requester:IWorldRequester)
    {
        orders.find(p => p.get1.key == stack.key && p.get2 == requester) match
        {
            case Some(p) =>
                p.get1.stackSize += stack.stackSize
                val idx = orders.indexOf(p)
                orders = orders.take(idx) ++ orders.drop(idx+1) :+ p
            case None => orders :+= new Pair2(stack, requester)
        }
        onOrdersChanged()
    }

    def dispatchSuccessful(amount:Int, restack:Boolean)
    {
        val first = orders.head
        first.get1.stackSize -= amount

        if (first.get1.stackSize <= 0) orders = orders.tail
        else if (restack) restackOrders()
    }

    def restackOrders()
    {
        orders = orders.tail :+ orders.head
    }

    def dispatchFailed()
    {
        val first = orders.head
        first.get2.itemLost(first.get1)
        if (orders.nonEmpty) orders = orders.tail
    }

    def peek =
    {
        if (orders.isEmpty) null
        else orders(0)
    }

    def hasOrders = orders.nonEmpty

    def getDeliveryCount(item:ItemKey) = orders.foldLeft(0)(
        (b, p) => b+(if (p.get1.key == item) p.get1.stackSize else 0))

    def getTotalDeliveryCount = orders.foldLeft(0)((b, p) => b+p.get1.stackSize)

    def onOrdersChanged(){}
}
