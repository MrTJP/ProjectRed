package mrtjp.projectred.expansion.graphs;

import javax.annotation.Nullable;

/**
 * Interface held by parts or tiles that can contain nodes. Nodes are used to build connection graphs within a
 * single network of connected elements.
 */
public interface GraphContainer {

    //region Linking
    GraphNode getNode();
    //endregion

    //region Path finding
    boolean canPropagate(int dir);

    @Nullable
    GraphContainer getNodeTowards(int dir);

    default int getLinkWeight() {
        return 1;
    }
    //endregion

    // True if this container's node does not need to be in final graph
    boolean requiresActiveNode();

    void onNodeChanged(boolean linksChanged, boolean stateChange);
    //endregion
}
