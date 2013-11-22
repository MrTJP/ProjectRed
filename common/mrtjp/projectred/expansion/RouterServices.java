package mrtjp.projectred.expansion;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import codechicken.lib.vec.BlockCoord;

public class RouterServices {

    public static RouterServices instance = new RouterServices();
    
    /** All registered routers **/
    private final ArrayList<Router> routers = new ArrayList<Router>();

    /** Map of [ID, IP] for all registered routers **/
    private final Map<UUID,Integer> UUIDTable = new HashMap<UUID, Integer>();

    public Router getRouter(int id) {
        if (id < 0 || id >= routers.size())
            return null;
        return routers.get(id);
    }
    
    public int getIPforUUID(UUID id){
        if(id == null)
            return -1;
        Integer simp = UUIDTable.get(id);
        if(simp == null)
            return -1;
        return simp.intValue();
    }
    
    public void removeRouter(int id) {
        routers.set(id, null);
    }

    public Router getOrCreateRouter(UUID uu, int dim, BlockCoord bc) {
        synchronized (routers) {
            for (Router r : routers)
                if (r != null && r.getDim() == dim && r.getLocation().equals(bc))
                    return r;
            
            Router r = new Router(uu, dim, bc);
            int simp = r.getIPAddress();
            
            if (routers.size() <= simp) {
                routers.ensureCapacity(simp+1);
                while(routers.size() <= simp)
                    routers.add(null);
            }
            routers.set(simp, r);
            UUIDTable.put(r.getID(), r.getIPAddress());
            return r;
        }
    }

    public boolean doesRouterExist(int id) {
        if (id < 0 || id >= routers.size())
            return false;
        
        return routers.get(id) != null;
    }
    
    public static void reboot() {
        instance = new RouterServices();
    }
}
