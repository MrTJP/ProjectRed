package mrtjp.projectred.fabrication.engine.gates;

import mrtjp.projectred.fabrication.editor.GatePlacementType;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateModelRenderer;

import static mrtjp.projectred.fabrication.editor.GatePlacementType.INTERNAL;
import static mrtjp.projectred.fabrication.editor.GatePlacementType.IO_EDGE;

public enum ICGateTileType {

    REDSTONE_IO         (ICTileType.REDSTONE_IO_GATE,           GateModelRenderer.getNonPartRenderIndex(0), IO_EDGE),
    BUNDLED_COLOR_IO    (ICTileType.BUNDLED_COLOR_IO_GATE,      GateModelRenderer.getNonPartRenderIndex(1), IO_EDGE),
    BUNDLED_BUS_IO      (ICTileType.BUNDLED_BUS_IO_GATE,        GateModelRenderer.getNonPartRenderIndex(2), IO_EDGE),
    ANALOG_IO           (ICTileType.ANALOG_IO_GATE,             GateModelRenderer.getNonPartRenderIndex(3), IO_EDGE),

    OR                   (ICTileType.OR_GATE,                   GateModelRenderer.getRenderIndex(GateType.OR)),
    NOR                  (ICTileType.NOR_GATE,                  GateModelRenderer.getRenderIndex(GateType.NOR)),
    NOT                  (ICTileType.NOT_GATE,                  GateModelRenderer.getRenderIndex(GateType.NOT)),
    AND                  (ICTileType.AND_GATE,                  GateModelRenderer.getRenderIndex(GateType.AND)),
    NAND                 (ICTileType.NAND_GATE,                 GateModelRenderer.getRenderIndex(GateType.NAND)),
    XOR                  (ICTileType.XOR_GATE,                  GateModelRenderer.getRenderIndex(GateType.XOR)),
    XNOR                 (ICTileType.XNOR_GATE,                 GateModelRenderer.getRenderIndex(GateType.XNOR)),
    BUFFER               (ICTileType.BUFFER_GATE,               GateModelRenderer.getRenderIndex(GateType.BUFFER)),
    MULTIPLEXER          (ICTileType.MULTIPLEXER_GATE,          GateModelRenderer.getRenderIndex(GateType.MULTIPLEXER)),
    PULSE                (ICTileType.PULSE_GATE,                GateModelRenderer.getRenderIndex(GateType.PULSE)),
    REPEATER             (ICTileType.REPEATER_GATE,             GateModelRenderer.getRenderIndex(GateType.REPEATER)),
    RANDOMIZER           (ICTileType.RANDOMIZER_GATE,           GateModelRenderer.getRenderIndex(GateType.RANDOMIZER)),
    SR_LATCH             (ICTileType.SR_LATCH_GATE,             GateModelRenderer.getRenderIndex(GateType.SR_LATCH)),
    TOGGLE_LATCH         (ICTileType.TOGGLE_LATCH_GATE,         GateModelRenderer.getRenderIndex(GateType.TOGGLE_LATCH)),
    TRANSPARENT_LATCH    (ICTileType.TRANSPARENT_LATCH_GATE,    GateModelRenderer.getRenderIndex(GateType.TRANSPARENT_LATCH)),
    TIMER                (ICTileType.TIMER_GATE,                GateModelRenderer.getRenderIndex(GateType.TIMER)),
    SEQUENCER            (ICTileType.SEQUENCER_GATE,            GateModelRenderer.getRenderIndex(GateType.SEQUENCER)),
    COUNTER              (ICTileType.COUNTER_GATE,              GateModelRenderer.getRenderIndex(GateType.COUNTER)),
    STATE_CELL           (ICTileType.STATE_CELL_GATE,           GateModelRenderer.getRenderIndex(GateType.STATE_CELL)),
    SYNCHRONIZER         (ICTileType.SYNCHRONIZER_GATE,         GateModelRenderer.getRenderIndex(GateType.SYNCHRONIZER)),
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
