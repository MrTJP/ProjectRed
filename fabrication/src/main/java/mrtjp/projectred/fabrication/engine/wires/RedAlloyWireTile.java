package mrtjp.projectred.fabrication.engine.wires;

import mrtjp.fengine.api.PropagationFunction;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.transmission.WireType;

public class RedAlloyWireTile extends RedstoneWireTile {

    public RedAlloyWireTile() {
        super(ICTileType.RED_ALLOY_WIRE, WireType.RED_ALLOY);
    }

    @Override
    protected int getRenderHue() {
        return (signal&0xFF)/2+60<<24|0xFF;
    }

    @Override
    protected int getTextureIndex() {
        return 0;
    }

    @Override
    public PropagationFunction propagationFunc(int inDir, int inPort) {
        // Accept any colour, forward to all colours
        return ((outDir, outPort) -> maskConnectsToDir(inDir) && maskConnectsToDir(outDir));
    }

}
