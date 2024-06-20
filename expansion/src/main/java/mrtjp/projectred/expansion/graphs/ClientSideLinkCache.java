package mrtjp.projectred.expansion.graphs;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Vector3;
import net.minecraft.Util;
import net.minecraft.core.BlockPos;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;

public class ClientSideLinkCache {

    public final List<ClientLink> links = new LinkedList<>();

    private boolean isActive = false;
    private @Nullable Consumer<List<ClientLink>> removedLinksCallback = null;
    private @Nullable Consumer<List<ClientLink>> addedLinksCallback = null;

    public boolean isActive() {
        return isActive;
    }

    public void setRemovedCallback(Consumer<List<ClientLink>> removedLinksCallback) {
        this.removedLinksCallback = removedLinksCallback;
    }

    public void setAddedCallback(Consumer<List<ClientLink>> addedLinksCallback) {
        this.addedLinksCallback = addedLinksCallback;
    }

    public void setLinks(List<GraphLink> links) {
        this.links.clear();
        for (var link : links) {
            this.links.add(new ClientLink(link.weight(), link.segments()));
        }
    }

    public void writeDesc(MCDataOutput packet) {
        packet.writeVarInt(links.size());
        for (var link : links) {
            link.writeDesc(packet);
        }
        packet.writeBoolean(isActive);
    }

    public void readDesc(MCDataInput packet) {
        links.clear();
        int size = packet.readVarInt();
        for (int i = 0; i < size; i++) {
            links.add(ClientLink.readDesc(packet));
        }
        isActive = packet.readBoolean();
    }

    public void setActive(boolean active) {
        isActive = active;
    }

    public void writeStateUpdate(MCDataOutput packet) {
        packet.writeBoolean(isActive);
    }

    public void writeLinkUpdate(MCDataOutput packet) {
        writeDesc(packet);
    }

    public void readStateUpdate(MCDataInput packet) {
        isActive = packet.readBoolean();
    }

    public void readLinkUpdate(MCDataInput packet) {
        List<ClientLink> oldList = new LinkedList<>(links);
        readDesc(packet);

        // Find added/removed links
        List<ClientLink> removed = new LinkedList<>();
        List<ClientLink> added = new LinkedList<>();
        for (var link : oldList) {
            if (!links.contains(link)) {
                removed.add(link);
            }
        }
        for (var link : links) {
            if (!oldList.contains(link)) {
                added.add(link);
            }
        }

        // Notify callbacks
        if (removedLinksCallback != null) {
            removedLinksCallback.accept(removed);
        }
        if (addedLinksCallback != null) {
            addedLinksCallback.accept(added);
        }
    }

    public static class ClientLink {

        public final int weight;
        public final List<GraphLinkSegment> segments;

        private final Function<BlockPos, LinkedList<Vector3>> pointListCache = Util.memoize(this::buildPointList);

        public ClientLink(int weight, List<GraphLinkSegment> segments) {
            this.weight = weight;
            this.segments = segments;
        }

        public void writeDesc(MCDataOutput packet) {
            packet.writeVarInt(weight);
            packet.writeVarInt(segments.size());
            for (var segment : segments) {
                packet.writeByte(segment.dir());
                packet.writeVarInt(segment.length());
            }
        }

        public static ClientLink readDesc(MCDataInput packet) {
            var weight = packet.readVarInt();
            int segmentSize = packet.readVarInt();
            var segments = new LinkedList<GraphLinkSegment>();
            for (int j = 0; j < segmentSize; j++) {
                int dir = packet.readUByte();
                int length = packet.readVarInt();
                segments.add(new GraphLinkSegment(dir, length));
            }
            return new ClientLink(weight, segments);
        }

        public LinkedList<Vector3> getPointListFor(BlockPos pos) {
            return pointListCache.apply(pos);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ClientLink that = (ClientLink) o;
            return weight == that.weight && Objects.equals(segments, that.segments);
        }

        @Override
        public int hashCode() {
            return Objects.hash(weight, segments);
        }

        private LinkedList<Vector3> buildPointList(BlockPos from) {
            LinkedList<Vector3> points = new LinkedList<>();
            points.add(new Vector3(from.getX() + 0.5, from.getY() + 0.5, from.getZ() + 0.5));

            for (var segment : segments) {
                var last = points.getLast();
                switch (segment.dir()) {
                    case 0 -> points.add(last.copy().add(0, -segment.length(), 0));
                    case 1 -> points.add(last.copy().add(0,  segment.length(), 0));
                    case 2 -> points.add(last.copy().add(0, 0, -segment.length()));
                    case 3 -> points.add(last.copy().add(0, 0,  segment.length()));
                    case 4 -> points.add(last.copy().add(-segment.length(), 0, 0));
                    case 5 -> points.add(last.copy().add(segment.length(), 0, 0));
                }
            }
            return points;
        }
    }
}
