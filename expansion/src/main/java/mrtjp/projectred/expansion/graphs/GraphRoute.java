package mrtjp.projectred.expansion.graphs;

import java.util.List;

public record GraphRoute(GraphNode start, GraphNode end, int weight, List<GraphRouteEdge> edges) {

    public int direction() {
        return edges.get(0).dir();
    }

    @Override
    public String toString() {
        return "GraphRoute{" +
                "start=" + start.hashCode() +
                ", end=" + end.hashCode() +
                ", weight=" + weight +
                ", edges=" + edges +
                '}';
    }
}
