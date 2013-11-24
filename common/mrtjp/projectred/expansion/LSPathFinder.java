package mrtjp.projectred.expansion;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.expansion.Router.StartEndPath;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class LSPathFinder {

    public HashMap<Router, StartEndPath> result;
    private int pipesVisited;

    private final int maxVisited;
    private final int maxLength;
    private final HashSet<BasicPipePart> setVisited = new HashSet<BasicPipePart>();

    private final Router LSAddresser;


    public LSPathFinder(IWorldRouter start, int maxVisited, int maxLength) {
        this(start, maxVisited, maxLength, ForgeDirection.UNKNOWN);
    }
    public LSPathFinder(IWorldRouter start, int maxVisited, int maxLength, ForgeDirection side) {
        this.maxVisited = maxVisited;
        this.maxLength = maxLength;
        this.LSAddresser = start.getRouter();
        result = getConnectedRoutingPipes(start.getContainer(), side);
    }

    private HashMap<Router, StartEndPath> getConnectedRoutingPipes(BasicPipePart start, ForgeDirection side) {
        HashMap<Router, StartEndPath> foundPipes = new HashMap<Router, StartEndPath>();

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

            foundPipes.put(wr.getRouter(), new StartEndPath(LSAddresser, wr.getRouter(), side.getOpposite().ordinal(), setVisited.size()));

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

            HashMap<Router, StartEndPath> result = getConnectedRoutingPipes(p, dir);
            for(Entry<Router, StartEndPath> pipeEntry : result.entrySet()) {
                pipeEntry.getValue().dirToFirstHop = dir.ordinal();
                StartEndPath foundPipe = foundPipes.get(pipeEntry.getKey());

                if (foundPipe == null) {
                    foundPipes.put(pipeEntry.getKey(), pipeEntry.getValue());
                    pipeEntry.getValue().distance += resistance;
                } else if (pipeEntry.getValue().distance + resistance < foundPipe.distance) {
                    foundPipes.put(pipeEntry.getKey(), pipeEntry.getValue());
                    pipeEntry.getValue().distance += resistance;
                }
            }
        }
        setVisited.remove(start);
        if (start instanceof IWorldRouter)
            for (StartEndPath e : foundPipes.values())
                e.start = ((IWorldRouter) start).getRouter();

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
