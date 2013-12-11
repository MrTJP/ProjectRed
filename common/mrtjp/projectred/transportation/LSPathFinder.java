package mrtjp.projectred.transportation;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;

import mrtjp.projectred.core.APIImpl;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.transportation.Router.StartEndPath;
import net.minecraft.tileentity.TileEntity;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class LSPathFinder
{
    private HashMap<Router, StartEndPath> result;
    private int pipesVisited;

    private final int maxVisited;
    private final int maxLength;
    private final HashSet<BasicPipePart> setVisited = new HashSet<BasicPipePart>();

    private final Router LSAddresser;

    public LSPathFinder(IWorldRouter start, int maxVisited, int maxLength)
    {
        this(start, maxVisited, maxLength, ForgeDirection.UNKNOWN);
    }

    public LSPathFinder(IWorldRouter start, int maxVisited, int maxLength, ForgeDirection side)
    {
        this.maxVisited = maxVisited;
        this.maxLength = maxLength;
        this.LSAddresser = start.getRouter();
        result = getConnectedRoutingPipes(start.getContainer(), side);
    }

    private HashMap<Router, StartEndPath> getConnectedRoutingPipes(BasicPipePart start, ForgeDirection side)
    {
        HashMap<Router, StartEndPath> foundPipes = new HashMap<Router, StartEndPath>();

        boolean root = setVisited.isEmpty();

        if (setVisited.size() == 1)
            pipesVisited = 0;

        if (setVisited.size() > maxLength)
            return foundPipes;

        if (!start.initialized)
            return foundPipes;

        if (start instanceof IWorldRouter && !root)
        {
            IWorldRouter wr = (IWorldRouter) start;
            if (wr.needsWork())
                return foundPipes;

            foundPipes.put(wr.getRouter(), new StartEndPath(LSAddresser, wr.getRouter(), side.getOpposite().ordinal(), setVisited.size()));

            return foundPipes;
        }

        setVisited.add(start);

        ArrayDeque<Pair2<TileEntity, ForgeDirection>> connections = new ArrayDeque<Pair2<TileEntity, ForgeDirection>>();

        for (ForgeDirection dir : ForgeDirection.VALID_DIRECTIONS)
        {
            if (root && side != ForgeDirection.UNKNOWN && !dir.equals(side))
                continue;

            if (!start.maskConnects(dir.ordinal()))
                continue;

            BlockCoord bc = new BlockCoord(start.tile()).offset(dir.ordinal());
            TMultiPart t = BasicUtils.getMultiPart(start.world(), bc, 6);

            if (!(t instanceof BasicPipePart))
                continue;
            BasicPipePart t2 = (BasicPipePart) t;

            connections.add(new Pair2<TileEntity, ForgeDirection>(t2.tile(), dir));
        }

        while (!connections.isEmpty())
        {
            Pair2<TileEntity, ForgeDirection> pair = connections.pollFirst();
            TileEntity te = pair.getValue1();
            ForgeDirection dir = pair.getValue2();

            if (root)
            {
                List<TileEntity> connected = APIImpl.getConnections(te);
                if (connected != null && !connected.isEmpty())
                {
                    for (TileEntity tile : connected)
                        connections.add(new Pair2<TileEntity, ForgeDirection>(tile, dir));
                    continue;
                }
            }

            TMultiPart part = BasicUtils.getMultiPart(te.worldObj, new BlockCoord(te), 6);
            BasicPipePart p = null;
            if (part instanceof BasicPipePart)
                p = (BasicPipePart) part;

            if (p == null)
                continue;

            if (setVisited.contains(p))
                continue;

            HashMap<Router, StartEndPath> result = getConnectedRoutingPipes(p, dir);

            for (Entry<Router, StartEndPath> entry : result.entrySet())
            {
                entry.getValue().dirToFirstHop = dir.ordinal();
                StartEndPath found = foundPipes.get(entry.getKey());

                if (found == null)
                    foundPipes.put(entry.getKey(), entry.getValue());
                else if (entry.getValue().distance < found.distance)
                    foundPipes.put(entry.getKey(), entry.getValue());
            }
        }
        setVisited.remove(start);
        if (start instanceof IWorldRouter)
            for (StartEndPath e : foundPipes.values())
                e.start = ((IWorldRouter) start).getRouter();

        return foundPipes;
    }

    public HashMap<Router, StartEndPath> getResult()
    {
        return result;
    }
}
