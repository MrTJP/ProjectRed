package mrtjp.projectred.expansion;

import java.util.BitSet;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.expansion.RoutedPayload.SendPriority;
import net.minecraft.item.ItemStack;

public interface IRouteLayer {

    public void queueStackToSend(ItemStack stack, int dirOfInventory, SyncResponse path);

    public void queueStackToSend(ItemStack stack, int dirToInventory, SendPriority priority, int destination);

    public SyncResponse getLogisticPath(ItemKey stack, BitSet exclusions, boolean excludeStart);

    public Router getRouter();

    public IWorldRouter getWorldRouter();

    public IWorldBroadcaster getBroadcaster();

    public IWorldRequester getRequester();
}