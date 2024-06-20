package mrtjp.projectred.expansion.graphs;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static mrtjp.projectred.expansion.ProjectRedExpansion.LOGGER;

public class GraphNode {

    public final GraphContainer container;

    private boolean isActive = false;

    private List<GraphLink> links = new LinkedList<>();
    private boolean linksNeedRefresh = false;

    private @Nullable GraphRouteTable routeTable;

    public GraphNode(GraphContainer container) {
        this.container = container;
    }

    public List<GraphLink> getLinks() {
        if (linksNeedRefresh) {
            // Meh kinda messy
            boolean wasActive = isActive;
            boolean linksChanged = refreshLinks();
            boolean stateChanged = wasActive == !isActive;
            linksNeedRefresh = false;
            if (linksChanged || stateChanged) {
                LOGGER.debug("Container {} node change: links: {}, state: {}", container.hashCode(), linksChanged, stateChanged);
                container.onNodeChanged(linksChanged, stateChanged);
            }
        }
        return links;
    }

    public GraphRouteTable getRouteTable() {
        if (routeTable == null) {
            routeTable = new GraphRoutePathfinder(this).result();
        }
        return routeTable;
    }

    public Optional<List<GraphLink>> getLinksIfPresent() {
        return linksNeedRefresh ? Optional.empty() : Optional.of(links);
    }

    public Optional<GraphRouteTable> getRouteTableIfPresent() {
        return routeTable == null ? Optional.empty() : Optional.of(routeTable);
    }

    public boolean isActive() {
        return isActive;
    }

    public void onTick() {
        // Refresh links if needed
        getLinks();
    }

    public void markLinksChanged() {
        linksNeedRefresh = true;
        routeTable = null;
    }

    public void markRouteTableChanged() {
        routeTable = null;
    }

    public void onAdded() {
        // Force refresh immediately
        getLinks();

        // If redundant, adjacent links must at least rebuild in case this
        // new element allows for new connections
        if (!isActive) {
            // Actively search and force-notify all adjacent links
            var pathfinder = new GraphLinkPathfinder(container);
            var result = pathfinder.result();
            LOGGER.debug("onAdded: Marking {} links for updates", result.size());
            notifyAllLinks(result);
        }
    }

    public int onRemoved() {
        // Actively search and force-notify all adjacent links
        var pathfinder = new GraphLinkPathfinder(container);
        var result = pathfinder.result();
        int sideMask = 0;
        for (GraphLink link : result) {
            sideMask |= 1 << link.direction();
        }
        LOGGER.debug("onRemoved: Marking {} links (mask {}) for updates", result.size(), sideMask);
        if (!result.isEmpty()) {
            notifyAllLinks(result);
        }
        return sideMask;
    }

    private boolean refreshLinks() {

        boolean wasActive = isActive;
        isActive = container.requiresActiveNode();

        if (!wasActive && isActive) { // Node is now active
            LOGGER.debug("node {} going active", this.container.hashCode());

            assert links.isEmpty(); // Should be empty if redundant

            var pathfinder = new GraphLinkPathfinder(container);
            List<GraphLink> newLinks = pathfinder.result();
            if (!newLinks.isEmpty()) {
                links = newLinks;
                invalidateRoutes(links);
                notifyAllLinks(links);
                return true;
            }

        } else if (wasActive && !isActive) { // Node is now inactive
            LOGGER.debug("node {} going inactive", this.container.hashCode());

            if (!links.isEmpty()) {
                var oldLinks = links;
                links = List.of();
                // Since we only notify new links, this is not needed?
//                invalidateRoutes(oldLinks);
                notifyAllLinks(oldLinks);
                return true;
            }

        } else if (isActive) { // Still active
            LOGGER.debug("node {} updating links", this.container.hashCode());

            var pathfinder = new GraphLinkPathfinder(container);
            List<GraphLink> newLinks = pathfinder.result();

            if (!linksEqual(links, newLinks)) {
                var oldLinks = links;
                links = newLinks;
                invalidateRoutes(links);
                notifyChangedLinks(oldLinks, links);
                return true;
            }
        }

        return false;
    }

    private boolean linksEqual(List<GraphLink> a, List<GraphLink> b) {
        if (a.size() != b.size()) return false;

        // User iterator to compare all elements
        var aIter = a.iterator();
        var bIter = b.iterator();
        while (aIter.hasNext()) {
            if (!aIter.next().equals(bIter.next())) return false;
        }
        return true;
    }

    private void notifyAllLinks(List<GraphLink> links) {
        for (GraphLink link : links) {
            LOGGER.debug("notifying link {}", link.to().hashCode());
            link.to().getNode().markLinksChanged();
        }
    }

    private void invalidateRoutes(List<GraphLink> links) {
        var allNodes = new GraphNodePathfinder(links.stream().map(l -> l.to().getNode()).toList()).result();
        var start = links.stream().mapToInt(l -> l.to().hashCode()).toArray();
        var res = allNodes.stream().mapToInt(n -> n.container.hashCode()).toArray();
        LOGGER.debug("invalidating routes for {} total nodes: {} -> {}", allNodes.size(), start, res);
        allNodes.forEach(GraphNode::markRouteTableChanged);
    }

    private void notifyChangedLinks(List<GraphLink> oldLinks, List<GraphLink> newLinks) {
        for (GraphLink link : oldLinks) {
            if (!newLinks.contains(link)) {
                LOGGER.debug("notifying removed link {}", link.to().hashCode());
                link.to().getNode().markLinksChanged();
            }
        }

        for (GraphLink link : newLinks) {
            if (!oldLinks.contains(link)) {
                LOGGER.debug("notifying added link {}", link.to().hashCode());
                link.to().getNode().markLinksChanged();
            }
        }
    }

    @Override
    public String toString() {
        return "GraphNode{" +
                "container=" + container.hashCode() +
                ", links=" + links +
                ", isRedundant=" + !isActive +
                ", linksNeedRefresh=" + linksNeedRefresh +
                '}';
    }
}
