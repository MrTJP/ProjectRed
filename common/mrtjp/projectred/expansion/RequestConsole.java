package mrtjp.projectred.expansion;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.expansion.RequestBranch.RequestFlags;
import net.minecraft.item.ItemStack;

public class RequestConsole {
    private EnumSet<RequestFlags> settings = EnumSet.noneOf(RequestFlags.class);

    private IWorldRequester destination = null;
    private RequestBranch branch = null;

    private int requested = 0;
    private Map<ItemKey, Integer> used = null;
    private Map<ItemKey, Integer> missing = null;

    public RequestConsole setPulling(boolean flag) {
        if (flag)
            settings.add(RequestFlags.PULL);
        else
            settings.remove(RequestFlags.PULL);
        return this;
    }
    public RequestConsole setCrafting(boolean flag) {
        if (flag)
            settings.add(RequestFlags.CRAFT);
        else
            settings.remove(RequestFlags.CRAFT);
        return this;
    }
    public RequestConsole setPartials(boolean flag) {
        if (flag)
            settings.add(RequestFlags.PARTIALS);
        else
            settings.remove(RequestFlags.PARTIALS);
        return this;
    }
    public RequestConsole setSimulate(boolean flag) {
        if (flag)
            settings.add(RequestFlags.SIMULATE);
        else
            settings.remove(RequestFlags.SIMULATE);
        return this;
    }
    public RequestConsole setDestination(IWorldRequester destination) {
        this.destination = destination;
        return this;
    }

    public int requested() {
        return requested;
    }

    public RequestConsole makeRequest(ItemStack request) {
        return makeRequest(ItemKeyStack.get(request));
    }

    public RequestConsole makeRequest(ItemKeyStack request) {
        if (destination == null)
            return this;

        parityBuilt = false;
        used = null;
        missing = null;
        requested = 0;

        branch = new RequestBranch(request.copy(), destination, settings);

        if (branch.isDone() || settings.contains(RequestFlags.PARTIALS) && branch.getPromisedCount() > 0) {
            requested = branch.getPromisedCount();

            if (!settings.contains(RequestFlags.SIMULATE))
                branch.recurse_RequestDelivery();
        }
        return this;
    }

    private boolean parityBuilt = false;

    private void rebuildParity() {
        if (!parityBuilt)
            branch.recurse_RebuildParityTree();
        parityBuilt = true;
    }

    private void gatherUsed() {
        if (used == null) {
            rebuildParity();
            used = new HashMap<ItemKey, Integer>();
            branch.recurse_GatherStatisticsUsed(used);
        }
    }

    private void gatherMissing() {
        if (missing == null) {
            rebuildParity();
            missing = new HashMap<ItemKey, Integer>();
            branch.recurse_GatherStatisticsMissing(missing);
        }
    }

    public Map<ItemKey, Integer> getUsed() {
        if (used == null)
            gatherUsed();
        return used;
    }
    public Map<ItemKey, Integer> getMissing() {
        if (missing == null)
            gatherMissing();
        return missing;
    }
}
