package mrtjp.projectred.expansion.graphs;

import java.util.*;

public class GraphRoutePathfinder {

    // Search collections
    private final Queue<GraphRoute> open = new PriorityQueue<>(Comparator.comparingInt(GraphRoute::weight));
    private final HashSet<GraphRoute> openSet = new HashSet<>();
    private final HashSet<GraphRoute> closedSet = new HashSet<>();

    // Destination to path map
    private final HashMap<GraphNode, List<GraphRoute>> routeMap = new HashMap<>();
    // Direction to paths map
    private final HashMap<Integer, List<GraphRoute>> directionMap = new HashMap<>();
    // All possible routes sorted by weight
    private final List<GraphRoute> routes = new LinkedList<>();
    // Destinations by weight
    private final List<GraphNode> destinations = new LinkedList<>();
    private final Set<GraphNode> destinationSet = new HashSet<>(); // For quick contains check. Maybe use LinkedHashSet?

    public GraphRoutePathfinder(GraphNode startNode) {
        openInitial(startNode);
    }

    private void openInitial(GraphNode startNode) {
        for (var link : startNode.getLinks()) {
            var route = beginRoute(startNode, link.to().getNode(), link.direction(), link.weight());
            open.add(route);
            openSet.add(route);
        }
    }

    private void openNext(GraphRoute prev) {

        if (isRouteRedundant(prev)) return;

        // Add prev to result
        routeMap.computeIfAbsent(prev.end(), k -> new LinkedList<>()).add(prev);
        directionMap.computeIfAbsent(prev.direction(), k -> new LinkedList<>()).add(prev);
        routes.add(prev);
        if (!destinationSet.contains(prev.end())) {
            destinations.add(prev.end());
        }

        // See if prev connects to more nodes
        for (var link : prev.end().getLinks()) {
            GraphRoute next = appendToRoute(prev, link.to().getNode(), link.direction(), link.weight());
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

    private boolean isRouteRedundant(GraphRoute path) {

        // Check if this route loops back to origin
        if (path.end() == path.start()) return true;

        // Check for alternative shorter route
        var prevRoute = routeMap.get(path.end());
        if (prevRoute != null) {
            for (var prevPath : prevRoute) {
                if (prevPath.weight() <= path.weight()) return true;
            }
        }

        return false;
    }

    public GraphRouteTable result() {
        while (!isFinished()) step(); // Finish if needed
        return new GraphRouteTable(routeMap, directionMap, destinations, routes);
    }

    private static GraphRoute beginRoute(GraphNode start, GraphNode end, int dir, int weight) {
        return new GraphRoute(start, end, weight, List.of(new GraphRouteEdge(start, end, dir, weight)));
    }

    private static GraphRoute appendToRoute(GraphRoute route, GraphNode next, int dir, int weight) {
        List<GraphRouteEdge> newEdges = new LinkedList<>(route.edges());
        newEdges.add(new GraphRouteEdge(route.end(), next, dir, weight));
        return new GraphRoute(route.start(), next, route.weight() + weight, newEdges);
    }
}
