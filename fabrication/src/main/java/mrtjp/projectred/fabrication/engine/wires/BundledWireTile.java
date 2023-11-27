package mrtjp.projectred.fabrication.engine.wires;

import mrtjp.fengine.api.PropagationFunction;
import mrtjp.projectred.fabrication.engine.IBundledConnectableICTile;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;
import mrtjp.projectred.fabrication.engine.IInsulatedConnectableICTile;
import mrtjp.projectred.transmission.WireType;

public class BundledWireTile extends WireTile implements IBundledConnectableICTile {

    public BundledWireTile(int colour) {
        super(colour == -1 ? ICTileType.BUNDLED_NEUTRAL_WIRE : ICWireTileType.BUNDLED_COLOURED[colour].tileType,
                WireType.values()[WireType.BUNDLED_WHITE.ordinal() + colour]);
    }

    @Override
    public int getBundledColour() {
        return getWireType().getColourIdx();
    }

    @Override
    public boolean canConnectTo(IConnectableICTile target, int towardsDir) {

        if (target instanceof IBundledConnectableICTile) {
            int targetColour = ((IBundledConnectableICTile) target).getBundledColour();
            int thisColour = getBundledColour();
            return targetColour == thisColour || targetColour == -1 || thisColour == -1; // -1 means neutral
        }

        if (target instanceof IInsulatedConnectableICTile)
            return true;

        return false;
    }

    @Override
    public PropagationFunction propagationFunc(int inDir, int inPort) {
        // Accept any colour, but only forward to that specific colour
        return ((outDir, outPort) ->
                outPort == inPort && maskConnectsToDir(inDir) && maskConnectsToDir(outDir));
    }
}
