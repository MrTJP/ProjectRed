package mrtjp.projectred.expansion;

import net.minecraftforge.common.ForgeDirection;

public class NodeLink implements Comparable<NodeLink> {
    public ForgeDirection outOrient;

    public int outDistance;

    public Router source;
    public final Router destination;

    public NodeLink(Router source, Router destination, int out, int length) {
        this.source = source;
        this.destination = destination;
        
        this.outOrient = ForgeDirection.getOrientation(out);
        
        this.outDistance = length;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof NodeLink) {
            NodeLink p = (NodeLink)o;
            return this.outOrient.equals(p.outOrient) &&
                    this.outDistance == p.outDistance;
        }
        return false;
    }

    @Override
    public int compareTo(NodeLink o) {
        int c = this.outDistance - o.outDistance;
        if (c != 0) return c;
        return destination.getIPAddress()-o.destination.getIPAddress();
    }

}
