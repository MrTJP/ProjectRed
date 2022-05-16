package mrtjp.projectred.fabrication.engine.wires;

import mrtjp.fengine.api.PropagationFunction;
import mrtjp.projectred.fabrication.engine.IBundledConnectableICTile;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;
import mrtjp.projectred.fabrication.engine.IInsulatedConnectableICTile;
import mrtjp.projectred.fabrication.engine.IRedstoneConnectableICTile;
import mrtjp.projectred.fabrication.engine.wires.ICWireTileType;
import mrtjp.projectred.fabrication.engine.wires.RedstoneWireTile;
import mrtjp.projectred.transmission.WireType;

public class InsulatedWireTile extends RedstoneWireTile implements IInsulatedConnectableICTile, IRedstoneConnectableICTile {

    public InsulatedWireTile(int colour) {
        super(ICWireTileType.INSULATED[colour].tileType, WireType.values()[WireType.INSULATED_WHITE.ordinal() + colour]);
    }

    @Override
    protected int getRenderHue() {
        return -1; // Insulated wires don't render hue
    }

    @Override
    public int getInsulatedColour() {
        return getWireType().getColourIdx();
    }

    @Override
    protected int getTextureIndex() {
        return signal != 0 ? 1 : 0;
    }

    @Override
    public boolean canConnectTo(IConnectableICTile target, int towardsDir) {
        if (target instanceof IInsulatedConnectableICTile)
            return ((IInsulatedConnectableICTile) target).getInsulatedColour() == getInsulatedColour();

        if (target instanceof IBundledConnectableICTile)
            return true;

        return super.canConnectTo(target, towardsDir);
    }

    @Override
    public PropagationFunction propagationFunc(int inDir, int inPort) {
        // Accept only matching colour, and forward only to that colour
        return ((outDir, outPort) ->
                inPort == getInsulatedColour() && outPort == inPort &&
                        maskConnectsToDir(inDir) && maskConnectsToDir(outDir));
    }
}
