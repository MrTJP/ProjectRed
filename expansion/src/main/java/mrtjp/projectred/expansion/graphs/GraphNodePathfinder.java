package mrtjp.projectred.expansion.graphs;

import java.util.*;

public class GraphNodePathfinder {

    private final Queue<GraphNode> open = new LinkedList<>();
    private final HashSet<GraphNode> openSet = new HashSet<>();
    private final HashSet<GraphNode> closedSet = new LinkedHashSet<>();

    public GraphNodePathfinder(Collection<GraphNode> startNodes) {
        openInitial(startNodes);
    }

    private void openInitial(Collection<GraphNode> startNodes) {
        for (var node : startNodes) {

            // Uniques only
            if (openSet.contains(node)) continue;

            open.add(node);
            openSet.add(node);
        }
    }

    private void openNext(GraphNode prev) {

        // Don't actively rebuild links of any node. If links are not present, they will
        // be rebuilt next tick and that other node will re-perform this search.
        var nextLinks = prev.getLinksIfPresent().orElse(Collections.emptyList());

        for (var link : nextLinks) {
            GraphNode next = link.to().getNode();

            if (!openSet.contains(next) && !closedSet.contains(next)) {
                open.add(next);
                openSet.add(next);
            }
        }
    }

    public void step() {
        if (open.isEmpty()) return;

        var next = open.poll();
        openSet.remove(next);

        openNext(next);
        closedSet.add(next);
    }

    public boolean isFinished() {
        return open.isEmpty();
    }

    public Collection<GraphNode> result() {
        while (!isFinished()) {
            step();
        }
        return Collections.unmodifiableCollection(closedSet);
    }
}
