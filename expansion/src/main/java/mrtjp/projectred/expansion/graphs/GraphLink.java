package mrtjp.projectred.expansion.graphs;

import java.util.LinkedList;
import java.util.List;

public record GraphLink(GraphContainer from, GraphContainer to, int weight, List<GraphLinkSegment> segments) {

    public GraphLink(GraphContainer from, GraphContainer to, int weight, List<GraphLinkSegment> segments) {
        this.from = from;
        this.to = to;
        this.weight = weight;
        this.segments = new LinkedList<>(segments);
    }

    public int direction() {
        return segments.get(0).dir();
    }

    @Override
    public String toString() {
        return "GraphLink{" +
                "from=" + from.hashCode() +
                ", to=" + to.hashCode() +
                ", weight=" + weight +
                ", segments=" + segments +
                '}';
    }
}
