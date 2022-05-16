package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.projectred.fabrication.editor.GatePlacementType;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.RenderGate;

import static mrtjp.projectred.fabrication.editor.GatePlacementType.IO_EDGE;
import static mrtjp.projectred.fabrication.editor.GatePlacementType.INTERNAL;

public enum ICGateTileType {
    IO(ICTileType.IO_GATE, RenderGate.getNonPartRenderIndex(0), IO_EDGE),


    OR(ICTileType.OR_GATE, RenderGate.getRenderIndex(GateType.OR)),
    NOR(ICTileType.NOR_GATE, RenderGate.getRenderIndex(GateType.NOR)),
    NOT(ICTileType.NOT_GATE, RenderGate.getRenderIndex(GateType.NOT)),
    AND(ICTileType.AND_GATE, RenderGate.getRenderIndex(GateType.AND)),
    NAND(ICTileType.NAND_GATE, RenderGate.getRenderIndex(GateType.NAND)),
    XOR(ICTileType.XOR_GATE, RenderGate.getRenderIndex(GateType.XOR)),
    XNOR(ICTileType.XNOR_GATE, RenderGate.getRenderIndex(GateType.XNOR)),
    BUFFER(ICTileType.BUFFER_GATE, RenderGate.getRenderIndex(GateType.BUFFER)),
    MULTIPLEXER(ICTileType.MULTIPLEXER_GATE, RenderGate.getRenderIndex(GateType.MULTIPLEXER)),
    PULSE(ICTileType.PULSE_GATE, RenderGate.getRenderIndex(GateType.PULSE)),
    REPEATER(ICTileType.REPEATER_GATE, RenderGate.getRenderIndex(GateType.REPEATER)),
    RANDOMIZER(ICTileType.RANDOMIZER_GATE, RenderGate.getRenderIndex(GateType.RANDOMIZER)),
    SR_LATCH(ICTileType.SR_LATCH_GATE, RenderGate.getRenderIndex(GateType.SR_LATCH)),
    TOGGLE_LATCH(ICTileType.TOGGLE_LATCH_GATE, RenderGate.getRenderIndex(GateType.TOGGLE_LATCH)),
    TRANSPARENT_LATCH(ICTileType.TRANSPARENT_LATCH_GATE, RenderGate.getRenderIndex(GateType.TRANSPARENT_LATCH));

    ;

    public final ICTileType tileType;
    public final int renderIndex;
    public final GatePlacementType placementType;

    ICGateTileType(ICTileType tileType, int renderIndex) {
        this(tileType, renderIndex, INTERNAL);
    }

    ICGateTileType(ICTileType tileType, int renderIndex, GatePlacementType placementType) {
        this.tileType = tileType;
        this.renderIndex = renderIndex;
        this.placementType = placementType;
    }
}
