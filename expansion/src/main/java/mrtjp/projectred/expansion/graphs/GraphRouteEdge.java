package mrtjp.projectred.expansion.graphs;

public record GraphRouteEdge(GraphNode from, GraphNode to, int dir, int weight) {

    @Override
    public String toString() {
        return "GraphRouteEdge{" +
                "from=" + from.hashCode() +
                ", to=" + to.hashCode() +
                ", dir=" + dir +
                ", weight=" + weight +
                '}';
    }
}
