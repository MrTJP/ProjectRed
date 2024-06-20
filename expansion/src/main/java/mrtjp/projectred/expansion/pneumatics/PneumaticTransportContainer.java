package mrtjp.projectred.expansion.pneumatics;

import mrtjp.projectred.expansion.part.PneumaticTubePayload;

public interface PneumaticTransportContainer {

    PneumaticTransport getPneumaticTransport();

    //region Payload transport callbacks
    void setOutputDirection(PneumaticTubePayload payload);

    void onPayloadChanged(int id, PneumaticTubePayload payload);

    boolean onPayloadReachedOutput(int id, PneumaticTubePayload payload);
    //endregion

    //region Endpoints
    boolean canItemEnterTube(PneumaticTubePayload payload, int side);

    boolean canItemExitTube(PneumaticTubePayload payload, int side, PneumaticTransportMode mode);
    //endregion

    //region Payload interface
    boolean insertPayload(int s, PneumaticTubePayload payload);
    //endregion
}
