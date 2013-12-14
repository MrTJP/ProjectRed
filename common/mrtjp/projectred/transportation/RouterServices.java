package mrtjp.projectred.transportation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import codechicken.lib.vec.BlockCoord;

public class RouterServices
{
    public static RouterServices instance = new RouterServices();

    /** All registered routers **/
    private final ArrayList<Router> routers = new ArrayList<Router>();

    /** Map of [ID, IP] for all registered routers **/
    private final Map<UUID, Integer> UUIDTable = new HashMap<UUID, Integer>();

    public Router getRouter(int id)
    {
        if (id < 0 || id >= routers.size())
            return null;
        return routers.get(id);
    }

    public int getIPforUUID(UUID id)
    {
        if (id == null)
            return -1;
        Integer simp = UUIDTable.get(id);
        if (simp == null)
            return -1;
        return simp.intValue();
    }

    public void removeRouter(int id)
    {
        routers.set(id, null);
    }

    public Router getOrCreateRouter(UUID uu, IWorldRouter holder)
    {
        synchronized (routers)
        {
            for (Router r : routers)
                if (r != null && r.getParent() == holder)
                    return r;

            Router r = new Router(uu, holder);
            int newLease = r.getIPAddress();

            if (routers.size() <= newLease)
            {
                int newLength = (int) (newLease * 1.5) + 1;

                routers.ensureCapacity(newLength);
                while (routers.size() <= newLength)
                    routers.add(null);
            }
            
            routers.set(newLease, r);
            UUIDTable.put(r.getID(), r.getIPAddress());
            return r;
        }
    }

    public boolean routerExists(int ip)
    {
        if (ip < 0 || ip >= routers.size())
            return false;

        return routers.get(ip) != null;
    }

    public static void reboot()
    {
        instance = new RouterServices();
    }
}
