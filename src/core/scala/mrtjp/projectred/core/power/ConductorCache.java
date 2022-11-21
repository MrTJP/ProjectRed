package mrtjp.projectred.core.power;

import mrtjp.projectred.core.IPowerConnectable;
import mrtjp.projectred.core.PowerConductor;

import java.lang.ref.WeakReference;
import java.util.HashMap;

public class ConductorCache {

    private final HashMap<Integer, WeakReference<PowerConductor>> cache = new HashMap<>();

    private final ConductorGetter getter;
    private final int size;

    public ConductorCache(ConductorGetter parent, int size) {
        this.getter = parent;
        this.size = size;
    }

    public PowerConductor getExternalConductor(int index) {

        // Check cache and return if exists
        WeakReference<PowerConductor> ref = cache.get(index);
        if (ref != null && ref.get() != null && ref.get().isValid()) {
            return ref.get();
        }

        // Otherwise, try to re-cache
        PowerConductor conductor = getter.get(index);
        if (conductor == null || !conductor.isValid()) {
            cache.remove(index);
            return null;
        }

        cache.put(index, new WeakReference<>(conductor));
        return conductor;
    }

    public void invalidateCache() {
        cache.clear();
    }

    @FunctionalInterface
    public interface ConductorGetter {
        PowerConductor get(int index);
    }
}
