package mrtjp.projectred.transportation;

import java.util.BitSet;
import java.util.List;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.transportation.Router.StartEndPath;
import net.minecraft.inventory.IInventory;

public class LogisticPathFinder
{
    private final Router source;
    private SyncResponse result;

    private BitSet exclusions = new BitSet();
    private BitSet visited;

    private boolean excludeSource;

    private ItemKey payload;

    public LogisticPathFinder(Router source, ItemKey payload)
    {
        this.source = source;
        this.payload = payload;
        this.visited = new BitSet(Router.getEndOfIPPool());
    }

    public LogisticPathFinder setExclusions(BitSet exclusions)
    {
        this.exclusions = exclusions;
        return this;
    }

    public LogisticPathFinder setExcludeSource(boolean excludeSource)
    {
        this.excludeSource = excludeSource;
        return this;
    }

    public SyncResponse getResult()
    {
        return result;
    }

    public LogisticPathFinder findBestResult()
    {
        SyncResponse bestResponse = new SyncResponse();
        int bestIP = -1;

        for (StartEndPath l : source.getRoutesByCost())
        {
            Router r = l.end;

            if (excludeSource && r.getIPAddress() == source.getIPAddress())
                continue;

            if (excludeSource && sharesInventory(source.getParent().getContainer(), r.getParent().getContainer()))
                continue;

            if (exclusions.get(r.getIPAddress()) || visited.get(r.getIPAddress()))
                continue;

            visited.set(r.getIPAddress());

            IWorldRouter parent = r.getParent();

            if (parent == null)
                continue;

            SyncResponse sync = parent.getSyncResponse(payload, bestResponse);

            if (sync != null)
                if (sync.priority.ordinal() > bestResponse.priority.ordinal())
                {
                    bestResponse = sync;
                    bestIP = r.getIPAddress();
                }
                else if (sync.priority.ordinal() == bestResponse.priority.ordinal() && sync.customPriority > bestResponse.customPriority)
                {
                    bestResponse = sync;
                    bestIP = r.getIPAddress();
                }
        }

        if (bestIP > -1)
            result = bestResponse.setResponder(bestIP);

        return this;
    }

    public static boolean sharesInventory(BasicPipePart pipe1, BasicPipePart pipe2)
    {
        if (pipe1 == null || pipe2 == null)
            return false;

        if (pipe1.tile().worldObj != pipe2.tile().worldObj)
            return false;

        List<IInventory> adjacent1 = pipe1.getConnectedInventories();
        List<IInventory> adjacent2 = pipe2.getConnectedInventories();

        for (IInventory inv1 : adjacent1)
            if (adjacent2.contains(inv1))
                return true;

        return false;
    }
}
