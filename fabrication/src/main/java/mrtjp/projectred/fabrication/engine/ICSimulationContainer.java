package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.fengine.simulate.ICSimulation;
import net.minecraft.nbt.CompoundTag;

public class ICSimulationContainer {

    private ICSimulation simulation = PRFabricationEngine.instance.deserializeSimulation(PRFabricationEngine.EMPTY_SIMULATION_SERIALIZED);

    /**
     * Inputs to the simulation indexed by rotation (0-3).
     * <p>
     * Each input is a mask of 16 bits, which represents one of the
     * 16 bundled colours inside the tile map.
     */
    private final short[] inputs = new short[] { 0, 0, 0, 0 };

    /**
     * Outputs from the simulation indexed by rotation (0-3).
     */
    private final short[] outputs = new short[] { 0, 0, 0, 0 };

    private long systemTime = 0L;

    public void setInput(int rotation, short mask) {
        inputs[rotation] = mask;
    }

    public int setInputs(short[] inputs) {

        int changeMask = 0;
        for (int r = 0; r < 4; r++) {
            if (this.inputs[r] != inputs[r]) {
                this.inputs[r]  = inputs[r];
                changeMask |= 1 << r;
            }
        }

        return changeMask;
    }

    public short getInput(int rotation) {
        return inputs[rotation];
    }

    public void setSystemTime(long systemTime) {
        this.systemTime = systemTime;
    }

    public long getSystemTime() {
        return systemTime;
    }

    public void progressTime(long time) {
        systemTime += time;
    }

    public short getOutput(int rotation) {
        return outputs[rotation];
    }

    public void setFlatMap(ICFlatMap flatMap) {
        this.simulation = new ICSimulation(flatMap);

        pushInputs(0xF);
        pushTime();

        simulation.computeAll(null);
    }

    public void save(CompoundTag tag) {
        tag.putString("simulation", PRFabricationEngine.instance.serializeSimulation(simulation));
        for (int i = 0; i < 4; i++) {
            tag.putShort("in" + i, inputs[i]);
            tag.putShort("out" + i, outputs[i]);
        }
        tag.putLong("systemTime", systemTime);
    }

    public void load(CompoundTag tag) {
        ICSimulation simulation = PRFabricationEngine.instance.deserializeSimulation(tag.getString("simulation"));
        if (simulation != null) {
            this.simulation = simulation;
        }
        for (int i = 0; i < 4; i++) {
            inputs[i] = tag.getShort("in" + i);
            outputs[i] = tag.getShort("out" + i);
        }
        systemTime = tag.getLong("systemTime");
    }

    public void writeDesc(MCDataOutput out) {
        //TODO write simulation log
    }

    public void readDesc(MCDataInput in) {
        // TODO read simulation log
    }

    public void pushInputs(int rmask) {

        for (int r = 0; r < 4; r++) {
            if ((rmask & (1 << r)) != 0) {
                for (int i = 0; i < 16; i++) {
                    int regId = PRFabricationEngine.inputRegisterId(r, i);
                    byte value = (inputs[r] & (1 << i)) != 0 ? (byte) 1 : 0;
                    simulation.queueRegByteVal(regId, value);
                }
            }
        }
    }

    public void pullInputs(int rmask) {

        for (int r = 0; r < 4; r++) {
            if ((rmask & (1 << r)) != 0) {
                inputs[r] = 0;
                for (int i = 0; i < 16; i++) {
                    int regId = PRFabricationEngine.inputRegisterId(r, i);
                    if (simulation.getRegByteVal(regId) != 0) inputs[r] |= (1 << i);
                }
            }
        }
    }

    public void pushTime() {
        simulation.queueRegLongVal(PRFabricationEngine.REG_TIME, 0, systemTime);
    }

    public void pullTime() {
        systemTime = simulation.getRegLongVal(PRFabricationEngine.REG_TIME, 0);
    }

    public int pullOutputs() {

        short[] newOutputs = new short[4];
        for (int r = 0; r < 4; r++) {
            for (int i = 0; i < 16; i++) {
                int regId = PRFabricationEngine.outputRegisterId(r, i);
                int value = simulation.getRegByteVal(regId) != 0 ? 1 : 0;
                newOutputs[r] |= value << i;
            }
        }

        int changeMask = 0;
        for (int r = 0; r < 4; r++) {
            if (outputs[r] != newOutputs[r]) {
                outputs[r] = newOutputs[r];
                changeMask |= (1 << r);
            }
        }
        return changeMask;
    }

    public byte pullRegisterValue(int regId) {
        return simulation.getRegByteVal(regId);
    }

    public short pullShortValue(int r1, int r0) {
        return simulation.getRegShortVal(r1, r0);
    }

    public short pullShortValue(int[] r, int offset) {
        return simulation.getRegShortVal(r, offset);
    }

    public int pullIntValue(int r3, int r2, int r1, int r0) {
        return simulation.getRegIntVal(r3, r2, r1, r0);
    }

    public int pullIntValue(int[] r, int offset) {
        return simulation.getRegIntVal(r, offset);
    }

    public long pullLongValue(int r7, int r6, int r5, int r4, int r3, int r2, int r1, int r0) {
        return simulation.getRegLongVal(r7, r6, r5, r4, r3, r2, r1, r0);
    }

    public long pullLongValue(int[] r, int offset) {
        return simulation.getRegLongVal(r, offset);
    }

    public void simulate() {
        simulation.propagate(null);
    }
}
