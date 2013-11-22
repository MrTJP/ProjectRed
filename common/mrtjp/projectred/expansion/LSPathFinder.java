package mrtjp.projectred.expansion;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.utils.Pair2;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class LSPathFinder {

    public HashMap<Router, NodeLink> result;
    private int pipesVisited;
    
    private final int maxVisited;
    private final int maxLength;
    private final HashSet<BasicPipePart> setVisited;
    

    public LSPathFinder(BasicPipePart start, int maxVisited, int maxLength) {
        this(maxVisited, maxLength);
        result = getConnectedRoutingPipes(start, ForgeDirection.UNKNOWN);
    }
    public LSPathFinder(BasicPipePart start, int maxVisited, int maxLength, ForgeDirection side) {
        this(maxVisited, maxLength);
        result = getConnectedRoutingPipes(start, side);
    }
    private LSPathFinder(int maxVisited, int maxLength) {
        this.maxVisited = maxVisited;
        this.maxLength = maxLength;
        this.setVisited = new HashSet<BasicPipePart>();
    }
    
    private HashMap<Router, NodeLink> getConnectedRoutingPipes(BasicPipePart start, ForgeDirection side) {
        HashMap<Router, NodeLink> foundPipes = new HashMap<Router, NodeLink>();

        boolean root = setVisited.isEmpty();
        
        if (setVisited.size() == 1)
            pipesVisited = 0;

        if (setVisited.size() > maxLength)
            return foundPipes;

        if (!start.initialized)
            return foundPipes;
        
        if (start instanceof IWorldRouter && !root) {
            IWorldRouter wr = (IWorldRouter)start;
            if (wr.needsWork())
                return foundPipes;
            
            foundPipes.put(wr.getRouter(), new NodeLink(null, wr.getRouter(), side.getOpposite().ordinal(), setVisited.size()));
            
            return foundPipes;
        }
        
        setVisited.add(start);
        
        ArrayDeque<Pair2<BasicPipePart, ForgeDirection>> connections = new ArrayDeque<Pair2<BasicPipePart,ForgeDirection>>();
        
        for (ForgeDirection dir : ForgeDirection.VALID_DIRECTIONS) {
            if(root && side != ForgeDirection.UNKNOWN && !dir.equals(side)) continue;
            
            if (!start.maskConnects(dir.ordinal())) continue;
            
            BlockCoord bc = new BlockCoord(start.tile()).offset(dir.ordinal());
            TMultiPart t = BasicUtils.getMultiPart(start.world(), bc, 6);
            
            if (!(t instanceof BasicPipePart)) continue;
            BasicPipePart t2 = (BasicPipePart)t;
                        
            connections.add(new Pair2<BasicPipePart, ForgeDirection>(t2, dir));            
        }
        
        while (!connections.isEmpty()) {
            Pair2<BasicPipePart, ForgeDirection> pair = connections.pollFirst();
            BasicPipePart p = pair.getValue1();
            ForgeDirection dir = pair.getValue2();
            int resistance = 0;

            if (setVisited.contains(p))
                continue;
            
            int beforeRecurseCount = foundPipes.size();
            HashMap<Router, NodeLink> result = getConnectedRoutingPipes(p, dir);
            for(Entry<Router, NodeLink> pipeEntry : result.entrySet()) {
                pipeEntry.getValue().outOrient = dir;
                NodeLink foundPipe = foundPipes.get(pipeEntry.getKey());
                if (foundPipe == null) {
                    foundPipes.put(pipeEntry.getKey(), pipeEntry.getValue());
                    pipeEntry.getValue().outDistance += resistance;
                } else if (pipeEntry.getValue().outDistance + resistance < foundPipe.outDistance) {
                    foundPipes.put(pipeEntry.getKey(), pipeEntry.getValue());
                    pipeEntry.getValue().outDistance += resistance;
                }
            }
        }
        setVisited.remove(start);
        if (start instanceof IWorldRouter)
            for (NodeLink e : foundPipes.values())
                e.source = ((IWorldRouter) start).getRouter();
        
        return foundPipes;
    }

    private int countConns(int connMap) {
        int conns = 0;
        for (int i = 0; i < 6; i++)
            if ((connMap & 1<<i) != 0)
                conns++;
        return conns;
    }
}
