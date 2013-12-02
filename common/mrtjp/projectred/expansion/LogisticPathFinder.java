package mrtjp.projectred.expansion;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.expansion.Router.StartEndPath;
import net.minecraft.inventory.IInventory;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;

public class LogisticPathFinder {

    private final Router source;
    private SyncResponse result;

    private BitSet exclusions = new BitSet();
    private BitSet visited;

    private boolean excludeSource;

    private ItemKey payload;

    public LogisticPathFinder(Router source, ItemKey payload) {
        this.source = source;
        this.payload = payload;
        this.visited = new BitSet(Router.getEndOfIPPool());
    }

    public LogisticPathFinder setExclusions(BitSet exclusions) {
        this.exclusions = exclusions;
        return this;
    }
    public LogisticPathFinder setExcludeSource(boolean excludeSource) {
        this.excludeSource = excludeSource;
        return this;
    }

    public SyncResponse getResult() {
        return result;
    }

    public LogisticPathFinder findBestResult() {
        SyncResponse bestResponse = new SyncResponse();
        int bestIP = -1;

        for (StartEndPath l : source.getRoutesByCost()) {
            Router r = l.end;

            if (excludeSource && r.getIPAddress() == source.getIPAddress())
                continue;

            if (exclusions.get(r.getIPAddress()) || visited.get(r.getIPAddress()))
                continue;

            visited.set(r.getIPAddress());

            IWorldRouter parent = r.getParent();

            if (parent == null) continue;

            SyncResponse sync = parent.getSyncResponse(payload, bestResponse);

            if (sync != null)
                if (sync.priority.ordinal() > bestResponse.priority.ordinal()) {
                    bestResponse = sync;
                    bestIP = r.getIPAddress();
                } else if (sync.priority.ordinal() == bestResponse.priority.ordinal() && sync.customPriority > bestResponse.customPriority) {
                    bestResponse = sync;
                    bestIP = r.getIPAddress();
                }
        }
        
        if (bestIP > -1)
            result = bestResponse.setResponder(bestIP);

        return this;
    }
    
    public static boolean sharesInventory(BasicPipePart pipe1, BasicPipePart pipe2) {
        World w = pipe1.tile().worldObj;
        if (w != pipe2.tile().worldObj)
            return false;
        
        List<IInventory> adjacent1 = new ArrayList<IInventory>(6);
        List<IInventory> adjacent2 = new ArrayList<IInventory>(6);
        
        BlockCoord bc1 = new BlockCoord(pipe1.tile());
        BlockCoord bc2 = new BlockCoord(pipe2.tile());
        
        for (int i = 0; i < 6; i++) {
            if (pipe1.maskConnects(i)) {
                IInventory inv = BasicUtils.getTileEntity(w, bc1.copy().offset(i), IInventory.class);
                if (inv != null)
                    adjacent1.add(inv);
            }
            if (pipe2.maskConnects(i)) {
                IInventory inv = BasicUtils.getTileEntity(w, bc2.copy().offset(i), IInventory.class);
                if (inv != null)
                    adjacent2.add(inv);
            }
        }
        
        for (IInventory inv1 : adjacent1)
            if (adjacent2.contains(inv1))
                return true;
        
        return false;
    }
}
