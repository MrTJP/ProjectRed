package mrtjp.projectred.expansion;

import java.util.EnumSet;

import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.expansion.RequestTree2.RequestFlags;
import net.minecraft.item.ItemStack;

public class RequestConsole {
    private EnumSet<RequestFlags> settings = EnumSet.noneOf(RequestFlags.class);        
    private boolean success;
    private int requestCount;
    private int missingCount;
    
    IWorldRoutedRequester destination = null;
    
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
    
    public RequestConsole setDestination(IWorldRoutedRequester destination) {
        this.destination = destination;
        return this;
    }

    public boolean success() {
        return success;
    }
    
    public int requested() {
        return requestCount;
    }
    
    public int missing() {
        return missingCount;
    }
    
    public RequestConsole makeRequest(ItemStack request) {
        return makeRequest(ItemKeyStack.get(request));
    }
    
    public RequestConsole makeRequest(ItemKeyStack request) {
        if (destination == null)
            return this;
        
        RequestTree2 tree = new RequestTree2(request, destination, settings);
        if (tree.isDone() || (settings.contains(RequestFlags.PARTIALS) && tree.getPromisedCount() > 0)) {
            success = true;
            requestCount = tree.getPromisedCount();
            missingCount = tree.getMissingCount();
            if (!settings.contains(RequestFlags.SIMULATE))
                tree.recurse_RequestDelivery();
        }
        return this;
    }
}
