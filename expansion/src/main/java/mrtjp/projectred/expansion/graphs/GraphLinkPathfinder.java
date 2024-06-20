package mrtjp.projectred.expansion.graphs;

import java.util.*;

public class GraphLinkPathfinder {

    private final Queue<Node> open = new LinkedList<>();
    private final HashSet<Node> openSet = new HashSet<>();
    private final HashSet<Node> closedSet = new HashSet<>();

    private final List<GraphLink> links = new LinkedList<>();

    private final GraphContainer startContainer;

    public GraphLinkPathfinder(GraphContainer startContainer) {
        this.startContainer = startContainer;
        openInitial(startContainer);
    }

    private void openInitial(GraphContainer startContainer) {

        // Queue up all adjacent directions for exploration
        for (int s = 0; s < 6; s++) {

            // Allow container to check for valid connection, etc
            if (!startContainer.canPropagate(s)) continue;

            GraphContainer nextContainer = startContainer.getNodeTowards(s);
            if (nextContainer == null) continue;

            Node next = new Node(nextContainer, s ^ 1, s, startContainer.getLinkWeight(), Collections.singletonList(s));

            if (!openSet.contains(next) && !closedSet.contains(next)) {
                open.add(next);
                openSet.add(next);
            }
        }
    }

    private void openNext(Node prev) {

        // Check if this node is terminal
        if (prev.container.getNode().isActive()) {
            links.add(new GraphLink(startContainer, prev.container, prev.weight, prev.calcSegments()));
            return; // Don't explore further
        }

        // Queue up all adjacent directions for exploration
        for (int s = 0; s < 6; s++) {
            // Don't go back the way we came
            if (s == prev.inputDir) continue;

            // Allow container to check for valid connection, etc
            if (!prev.container.canPropagate(s)) continue;

            GraphContainer nextContainer = prev.container.getNodeTowards(s);
            if (nextContainer == null) continue;

            Node next = prev.towards(nextContainer, s, prev.container.getLinkWeight());

            if (!openSet.contains(next) && !closedSet.contains(next)) {
                open.add(next);
                openSet.add(next);
            }
        }
    }

    public void step() {
        if (open.isEmpty()) return;

        Node next = open.poll();
        openSet.remove(next);

        openNext(next);

        closedSet.add(next);
    }

    public boolean isFinished() {
        return open.isEmpty();
    }

    public List<GraphLink> result() {
        while (!isFinished()) step(); // Finish if needed
        return Collections.unmodifiableList(links);
    }

    private static class Node {
        public final GraphContainer container;
        public final int inputDir;
        public final int initialDir;
        public final int weight;
        public final List<Integer> path;

        public Node(GraphContainer container, int inputDir, int initialDir, int weight, List<Integer> path) {
            this.container = container;
            this.inputDir = inputDir;
            this.initialDir = initialDir;
            this.weight = weight;
            this.path = path;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Node node = (Node) o;
            return inputDir == node.inputDir && container == node.container;
        }

        @Override
        public int hashCode() {
            return Objects.hash(container, inputDir);
        }

        public Node towards(GraphContainer nextContainer, int dir, int weight) {
            List<Integer> nextPath = new LinkedList<>(path);
            nextPath.add(dir);
            return new Node(nextContainer, dir ^ 1, initialDir, this.weight + weight, nextPath);
        }

        public List<GraphLinkSegment> calcSegments() {
            // Using list of directions, turn them into segments which are
            // a pair of direction and distance

            if (path.isEmpty()) return Collections.emptyList();

            Iterator<Integer> it = path.iterator();
            int dir = it.next();
            int dist = 1;

            List<GraphLinkSegment> segments = new LinkedList<>();
            while (it.hasNext()) {
                int next = it.next();

                // If direction changed...
                if (next != dir) {
                    // Add the current segment
                    segments.add(new GraphLinkSegment(dir, dist));

                    // Begin a new segment
                    dir = next;
                    dist = 1;

                } else {
                    dist++;
                }
            }

            // Add the last segment
            segments.add(new GraphLinkSegment(dir, dist));

            return segments;
        }
    }
}
