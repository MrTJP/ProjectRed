package mrtjp.projectred.expansion;

import mrtjp.projectred.core.utils.ItemKey;

public interface IWorldRouter {

    public Router getRouter();

    public boolean needsWork();

    public boolean refreshState();

    public BasicPipePart getContainer();

    /** Item Syncing **/
    public void itemEnroute(RoutedPayload r);
    public void itemArrived(RoutedPayload r);
    public SyncResponse getSyncResponse(ItemKey item, SyncResponse rival);

    /** Item Requesting **/
    // Handled via subclasses.

    /** Item Broadcasting **/
    // Handled via subclasses.

    /** Item Crafting **/
    // Handled via subclasses.
}
