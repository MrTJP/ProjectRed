package mrtjp.projectred.transportation;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.core.utils.Pair2;

import java.util.LinkedList;

public class DeliveryManager
{
    private LinkedList<Pair2<ItemKeyStack, IWorldRequester>> orders = new LinkedList<Pair2<ItemKeyStack, IWorldRequester>>();

    public void addOrder(ItemKeyStack stack, IWorldRequester requester)
    {
        for (Pair2<ItemKeyStack, IWorldRequester> request : orders)
            if (request.get1() == stack && request.get2() == requester)
            {
                stack.setSize(stack.stackSize() + request.get1().stackSize());
                orders.remove(request);
                break;
            }
        orders.addLast(new Pair2<ItemKeyStack, IWorldRequester>(stack, requester));
        onOrdersChanged();
    }

    public void dispatchSuccessful(int amountSent, boolean reStack)
    {
        Pair2<ItemKeyStack, IWorldRequester> first = orders.getFirst();
        first.get1().setSize(first.get1().stackSize() - amountSent);

        if (first.get1().stackSize() <= 0)
            orders.removeFirst();
        else if (reStack)
            restock();
    }

    public void dispatchFailed()
    {
        Pair2<ItemKeyStack, IWorldRequester> first = orders.getFirst();
        first.get2().trackedItemLost(first.get1());
        if (!orders.isEmpty())
            orders.removeFirst();
        onOrdersChanged();
    }

    public void restock()
    {
        Pair2<ItemKeyStack, IWorldRequester> p = orders.removeFirst();
        orders.addLast(p);
    }

    public Pair2<ItemKeyStack, IWorldRequester> peek()
    {
        if (orders.isEmpty())
            return null;

        return orders.getFirst();
    }

    public boolean hasOrders()
    {
        return !orders.isEmpty();
    }

    public int getDeliveryCount(ItemKey item)
    {
        int count = 0;
        for (Pair2<ItemKeyStack, IWorldRequester> order : orders)
            if (order.get1().key().equals(item))
                count += order.get1().stackSize();
        return count;
    }

    public int getTotalDeliveryCount()
    {
        int count = 0;
        for (Pair2<ItemKeyStack, IWorldRequester> request : orders)
            count += request.get1().stackSize();

        return count;
    }

    public void onOrdersChanged()
    {
    }
}
