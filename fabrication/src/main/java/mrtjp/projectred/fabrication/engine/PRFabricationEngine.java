package mrtjp.projectred.fabrication.engine;

import mrtjp.fengine.api.FabricationEngine;
import mrtjp.fengine.api.ICAssembler;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.fengine.simulate.ByteRegister;
import mrtjp.fengine.simulate.ICGate;
import mrtjp.fengine.simulate.ICSimulation;
import mrtjp.fengine.simulate.StaticByteRegister;
import mrtjp.projectred.fabrication.engine.gates.*;

import java.util.HashMap;
import java.util.Map;

public class PRFabricationEngine extends FabricationEngine {

    // Static register allocations
    public static final int REG_IN_BASE = 0;
    public static final int REG_OUT_BASE = 64;

    public static final int REG_TIME_7 = 128; // Upper 8 bits of time
    public static final int REG_TIME_6 = 129;
    public static final int REG_TIME_5 = 130;
    public static final int REG_TIME_4 = 131;
    public static final int REG_TIME_3 = 132;
    public static final int REG_TIME_2 = 133;
    public static final int REG_TIME_1 = 134;
    public static final int REG_TIME_0 = 135; // Lower 8 bits of time

    public static final int REG_ZERO = 136;
    public static final int REG_ONE = 137;

    public static final int[] REG_TIME = { REG_TIME_7, REG_TIME_6, REG_TIME_5, REG_TIME_4, REG_TIME_3, REG_TIME_2, REG_TIME_1, REG_TIME_0 };

    public static final PRFabricationEngine instance = new PRFabricationEngine();

    public static final ICFlatMap EMPTY_FLAT_MAP = instance.newAssembler().result();

    public static final String EMPTY_FLAT_MAP_SERIALIZED = instance.serializeFlatMap(EMPTY_FLAT_MAP);
    public static final String EMPTY_SIMULATION_SERIALIZED = instance.serializeSimulation(new ICSimulation(EMPTY_FLAT_MAP));

    // Implementation-specific compilation format. A compiled flatmap is expected to have certain static registers and such
    // in order to function. Additionally, gate classes are expected to be forward compatible. If any breaking changes need
    // to be made, this format number should be incremented. Any incompatible compile formats should be dropped and players
    // should be alerted. Example:
    // * Gates already in-world with compiled designs should stop functioning with a clearly visible reason why
    // * IC Blueprints with compiled designs should notify player that they must be re-compiled in the workbench
    // * Etc.
    public static final int COMPILE_FORMAT = 1;

    public static int inputRegisterId(int r, int i) {
        return REG_IN_BASE + r * 16 + i;
    }

    public static int outputRegisterId(int r, int i) {
        return REG_OUT_BASE + r * 16 + i;
    }

    @Override
    public ICAssembler newAssembler() {
        ICAssembler assembler = super.newAssembler();
        addStaticRegisters(assembler);
        return assembler;
    }

    @Override
    public ICStepThroughAssembler newStepThroughAssembler() {
        ICStepThroughAssembler assembler = super.newStepThroughAssembler();
        addStaticRegisters(assembler);
        return assembler;
    }

    private void addStaticRegisters(ICAssembler assembler) {
        // Add Static registers
        for (int i = 0; i < 16; i++) {
            for (int r = 0; r < 4; r++) {
                assembler.addRegister(inputRegisterId(r, i), new ByteRegister());
                assembler.addRegister(outputRegisterId(r, i), new ByteRegister());
            }
        }

        // Add time registers
        for (int i = 0; i < 8; i++) {
            assembler.addRegister(REG_TIME[i], new ByteRegister());
        }

        // Add zero and one registers
        assembler.addRegister(REG_ZERO, new StaticByteRegister((byte) 0));
        assembler.addRegister(REG_ONE, new StaticByteRegister((byte) 1));
    }

    @Override
    public Map<Class<? extends ICGate>, String> getGateSerializationMap() {
        Map<Class<? extends ICGate>, String> map = new HashMap<>();

        map.put(ORGateTile.ORGate.class, "pr_or"); //TODO create an enum to keep track of these. Label must be permanent to avoid breaking saves.
        map.put(NORGateTile.NORGate.class, "pr_nor");
        map.put(NOTGateTile.NOTGate.class, "pr_not");
        map.put(ANDGateTile.ANDGate.class, "pr_and");
        map.put(NANDGateTile.NANDGate.class, "pr_nand");
        map.put(XORGateTile.XORGate.class, "pr_xor");
        map.put(XNORGateTile.XNORGate.class, "pr_xnor");
        map.put(BufferGateTile.BufferGate.class, "pr_buff");
        map.put(MultiplexerGateTile.MultiplexerGate.class, "pr_mlpx");
        map.put(PulseGateTile.PulseGate.class, "pr_pulse");
        map.put(RepeaterGateTile.RepeaterGate.class, "pr_repeater");
        map.put(RandomizerGateTile.RandomizerGate.class, "pr_rand");
        map.put(SRLatchGateTile.SRLatchGate.class, "pr_srlatch");
        map.put(ToggleLatchGateTile.ToggleLatchGate.class, "pr_tglatch");
        map.put(TransparentLatchGateTile.TransparentLatchGate.class, "pr_trlatch");
        map.put(TimerGateTile.TimerGate.class, "pr_timer");
        map.put(SequencerGateTile.SequencerGate.class, "pr_seq");
        map.put(CounterGateTile.CounterGate.class, "pr_counter");
        map.put(StateCellGateTile.StateCellGate.class, "pr_statecell");
        map.put(SynchronizerGateTile.SynchronizerGate.class, "pr_sync");

        return map;
    }
}
