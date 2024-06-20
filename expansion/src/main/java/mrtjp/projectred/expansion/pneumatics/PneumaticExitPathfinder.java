package mrtjp.projectred.expansion.pneumatics;

import mrtjp.projectred.expansion.graphs.GraphRouteTable;
import mrtjp.projectred.expansion.part.PneumaticTubePayload;

import javax.annotation.Nullable;
import java.util.List;

public class PneumaticExitPathfinder {

    private final PneumaticTransportContainer startContainer;
    private final GraphRouteTable routeTable;
    private final PneumaticTubePayload payload;
    private final int dirMask;
    private final List<PneumaticTransportMode> searchModes;

    // Search results
    boolean searched = false;
    private int exitDirMask = 0;
    private int exitWeight = Integer.MAX_VALUE;
    private @Nullable PneumaticTransportMode exitMode = null;

    public PneumaticExitPathfinder(PneumaticTransportContainer startContainer, GraphRouteTable routeTable, PneumaticTubePayload payload, int dirMask) {
        this(startContainer, routeTable, payload, dirMask, List.of(PneumaticTransportMode.values()));
    }

    public PneumaticExitPathfinder(PneumaticTransportContainer startContainer, GraphRouteTable routeTable, PneumaticTubePayload payload, int dirMask, List<PneumaticTransportMode> modes) {
        this.startContainer = startContainer;
        this.routeTable = routeTable;
        this.payload = payload;
        this.dirMask = dirMask;
        this.searchModes = modes;
    }

    private void search(PneumaticTransportContainer startContainer, int dirMask) {
        for (var mode: searchModes) {
            // Reset search
            exitDirMask = 0;
            exitWeight = Integer.MAX_VALUE;
            exitMode = null;

            // Search this mode
            if (searchMode(startContainer, dirMask, mode)) {
                exitMode = mode;
                break;
            }
        }

        searched = true;
    }

    private boolean searchMode(PneumaticTransportContainer startContainer, int dirMask, PneumaticTransportMode mode) {
        // First check for immediate exists out of starting container
        for (int s = 0; s < 6; s++) {
            if ((dirMask & (1 << s)) == 0) continue;

            // Check if any exits are available on the local tube itself
            if (startContainer.canItemExitTube(payload, s, mode)) {
                exitDirMask |= (1 << s);
                exitWeight = 0;
            }
        }

        // If immediate exit found, nothing better will be found in the route table
        if (exitDirMask != 0) {
            return true;
        }

        // Check route table for routes in each direction
        for (int s = 0; s < 6; s++) {
            if ((dirMask & (1 << s)) == 0) continue;

            // Check route table for exists
            var routeIt = routeTable.routeIteratorInDirection(s);
            while (routeIt.hasNext()) {
                var route = routeIt.next();
                if (!(route.end().container instanceof PneumaticTransportContainer ptc)) continue;

                if (route.weight() > exitWeight) break; // Routes are weight-sorted, so nothing better will be found

                // See if this destination can accept the item
                for (int s1 = 0; s1 < 6; s1++) {
                    if (ptc.canItemExitTube(payload, s1, mode)) {
                        // If this route is shortest so far, clear all other directions
                        if (route.weight() < exitWeight) {
                            exitWeight = route.weight();
                            exitDirMask = 0;
                        }
                        exitDirMask |= (1 << s);
                        break;
                    }
                }
            }
        }

        return exitDirMask != 0;
    }

    public PneumaticExits result() {
        if (!searched) search(startContainer, dirMask);
        return new PneumaticExits(exitDirMask, exitWeight, exitMode);
    }

    public record PneumaticExits(int exitDirMask, int weight, @Nullable PneumaticTransportMode mode) {}
}
