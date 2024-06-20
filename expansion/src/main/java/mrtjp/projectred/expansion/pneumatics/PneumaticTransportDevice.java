package mrtjp.projectred.expansion.pneumatics;

import mrtjp.projectred.expansion.part.PneumaticTubePayload;

public interface PneumaticTransportDevice {

    boolean canConnectTube(int s);

    boolean canAcceptPayload(int s, PneumaticTubePayload payload, PneumaticTransportMode mode);

    boolean insertPayload(int s, PneumaticTubePayload payload);
}
