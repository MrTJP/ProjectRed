package mrtjp.projectred.integration.part;

import net.minecraft.core.BlockPos;

public interface IGateRenderData {

    //@formatter:off
    // Render type
    int getRenderIndex(); // Unique identifier that selects exact model to render
    int getOrientation(); // Packed orientation data. See {@link VecLib.orientT}

    // General stuff
    int shape();
    int state();
    default int state2() { return 0; }

    // Timer-like gates with pointers
    default boolean isPointerStarted() { return false; }
    default int pointerValue() { return 0;}
    default int pointerMax() { return 1; }

    // Bundled gates
    default short bOutput0() { return 0; }
    default short bOutput1() { return 0; }
    default short bOutput2() { return 0; }
    default short bOutput3() { return 0; }
    default short bInput0() { return 0; }
    default short bInput1() { return 0; }
    default short bInput2() { return 0; }
    default short bInput3() { return 0; }

    // Bus converter
    default int rsIO() { return 0; }

    // Segment display
    default int bInHigh() { return 0; } //TODO this is just bundled input side 0
    default byte segmentColour() { return 0; } // TODO this can be a state2

    // Bus input panel
    default BlockPos worldPos() { return BlockPos.ZERO; }

    // Array Cells
    default byte bottomSignal() { return 0; }
    default byte topSignal() { return 0; }
    default int topSignalConnMask() { return 0; }

    // Fabricated Gate
    default String getGateName() { return "???"; }
    default boolean hasRuntimeError() { return false; }
    //@formatter:on
}
