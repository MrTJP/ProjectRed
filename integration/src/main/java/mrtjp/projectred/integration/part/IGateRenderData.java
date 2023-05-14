package mrtjp.projectred.integration.part;

public interface IGateRenderData {

    //@formatter:off
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

    // Array Cells
    default byte bottomSignal() { return 0; }
    default byte topSignal() { return 0; }
    default int topSignalConnMask() { return 0; }
    //@formatter:on
}
