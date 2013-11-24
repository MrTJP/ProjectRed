package mrtjp.projectred.expansion;

import mrtjp.projectred.expansion.RoutedPayload.SendPriority;

public class SyncResponse {

    SendPriority priority = SendPriority.WANDERING;
    int customPriority = 0;
    int itemCount = 0;
    int responder = -1;

    public SyncResponse() {

    }

    public SyncResponse setPriority(SendPriority priority) {
        this.priority = priority;
        return this;
    }
    public SyncResponse setCustomPriority(int customPriority){
        this.customPriority = customPriority;
        return this;
    }
    public SyncResponse setItemCount(int itemCount) {
        this.itemCount = itemCount;
        return this;
    }
    public SyncResponse setResponder(int responder) {
        this.responder = responder;
        return this;
    }
}
