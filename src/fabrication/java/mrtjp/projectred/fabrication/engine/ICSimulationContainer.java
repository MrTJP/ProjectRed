package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.fengine.simulate.ICSimulation;
import net.minecraft.nbt.CompoundNBT;

public class ICSimulationContainer {

    private String flatMap = PRFabricationEngine.EMPTY_FLAT_MAP_SERIALIZED; //TODO this does not really belong here
    private ICSimulation simulation = PRFabricationEngine.EMPTY_SIMULATION;

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

    public void progressTime(long time) {
        systemTime += time;
    }

    public short getOutput(int rotation) {
        return outputs[rotation];
    }

    public void setFlatMap(ICFlatMap flatMap) {
        this.flatMap = PRFabricationEngine.instance.serializeFlatMap(flatMap);
        this.simulation = new ICSimulation(flatMap);

        pushInputs(0xF);
        pushTime();

        simulation.computeAll(null);
    }

    public void save(CompoundNBT tag) {
        tag.putString("flatMap", flatMap);
        tag.putString("simulation", PRFabricationEngine.instance.serializeSimulation(simulation));
        for (int i = 0; i < 4; i++) {
            tag.putShort("in" + i, inputs[i]);
            tag.putShort("out" + i, outputs[i]);
        }
        tag.putLong("systemTime", systemTime);
    }

    public void load(CompoundNBT tag) {
        this.flatMap = tag.getString("flatMap");
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

        simulation.queueRegByteVal(PRFabricationEngine.REG_TIME_3, (byte) (systemTime >>> 24));
        simulation.queueRegByteVal(PRFabricationEngine.REG_TIME_2, (byte) (systemTime >>> 16));
        simulation.queueRegByteVal(PRFabricationEngine.REG_TIME_1, (byte) (systemTime >>> 8));
        simulation.queueRegByteVal(PRFabricationEngine.REG_TIME_0, (byte) (systemTime));
    }

    public void pullTime() {

        systemTime = 0;
        systemTime |= (long) (simulation.getRegByteVal(PRFabricationEngine.REG_TIME_3) & 0xFF) << 24;
        systemTime |= (simulation.getRegByteVal(PRFabricationEngine.REG_TIME_2) & 0xFF) << 16;
        systemTime |= (simulation.getRegByteVal(PRFabricationEngine.REG_TIME_1) & 0xFF) << 8;
        systemTime |= (simulation.getRegByteVal(PRFabricationEngine.REG_TIME_0) & 0xFF);
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

    public void simulate() {
        simulation.propagate(null);
    }
}
