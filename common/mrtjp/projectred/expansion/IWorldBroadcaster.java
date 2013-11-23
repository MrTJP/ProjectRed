package mrtjp.projectred.expansion;

import java.util.Map;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.expansion.RequestTreeNode2.DeliveryPromise;

public interface IWorldBroadcaster extends IWorldRouter {

    public void requestPromises(RequestTreeNode2 request, int existingPromises);

    public void deliverPromises(DeliveryPromise promise, IWorldRequester requester);

    public void getBroadcastedItems(Map<ItemKey, Integer> map);
}
