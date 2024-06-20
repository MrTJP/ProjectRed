package mrtjp.projectred.expansion.graphs;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

public class GraphRouteTable {

    // Map of destinations and all paths to get there. Paths are sorted by weight
    private final HashMap<GraphNode, List<GraphRoute>> table;

    // Weight-sorted routes by hop direction
    private final HashMap<Integer, List<GraphRoute>> directions;

    // List of all unique destinations sorted by shortest path
    private final List<GraphNode> destinations;

    // List of all routes sorted by weight
    private final List<GraphRoute> routes;


    public GraphRouteTable(HashMap<GraphNode, List<GraphRoute>> table, HashMap<Integer, List<GraphRoute>> directions, List<GraphNode> destinations, List<GraphRoute> routes) {
        this.table = table;
        this.directions = directions;
        this.destinations = destinations;
        this.routes = routes;
    }

    public List<GraphRoute> getPathsTo(GraphNode destination) {
        return table.get(destination);
    }

    public Iterator<GraphRoute> routeIteratorInDirection(int direction) {
        if (!directions.containsKey(direction)) {
            return Collections.emptyIterator();
        }
        return directions.get(direction).iterator();
    }

    public List<GraphNode> getDestinations() {
        return destinations;
    }

    public Iterator<GraphRoute> routeIterator() {
        return routes.iterator();
    }
}
