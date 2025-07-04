package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.IPathFinder;
import mrtjp.fengine.assemble.PathFinderResult;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.IIOConnectionTile;
import mrtjp.projectred.fabrication.engine.IRotatableICTile;
import mrtjp.projectred.fabrication.engine.log.MultipleDriversError;
import net.minecraft.nbt.CompoundTag;

import java.util.Optional;

import static mrtjp.projectred.fabrication.engine.PRFabricationEngine.*;

public abstract class SingleBitIOGateTile extends RedstoneGateTile implements IIOConnectionTile {

    public static final int IO_BIT_PACKET = 7;

    protected byte ioBit = 0;
    private int regId = REG_ZERO;

    public SingleBitIOGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    //region Save/Load
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putInt("reg", regId);
        tag.putByte("io_bit", ioBit);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        regId = tag.getInt("reg");
        ioBit = tag.getByte("io_bit");
    }
    //endregion

    //region Network
    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
        out.writeByte(ioBit);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
        ioBit = in.readByte();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case IO_BIT_PACKET -> ioBit = in.readByte();
            default -> super.read(in, key);
        }
    }

    protected void sendIOBitUpdate() {
        getWriteStream(IO_BIT_PACKET).writeByte(ioBit);
    }
    //endregion

    protected void toggleWorldInput() {
        getEditor().getStateMachine().onInputRegistersChanged(getIOSide(), this::toggleWorldInputMask);
    }

    protected short toggleWorldInputMask(short currentMask) {
        // Default implementation just toggles the corresponding bit
        return (short) (currentMask ^ (1 << ioBit));
    }

    protected void shiftIOBit(boolean up) {
        ioBit = (byte) ((ioBit + (up ? 1 : 15)) % 16);
        sendIOBitUpdate();
        getEditor().markTileChange();
    }

    protected void toggleDirection() {
        configureShapeAndSend((getShape() + 1) % 2);
    }

    protected int getStaticOutputRegister(int ioBit) {
        return outputRegisterId(getIOSide(), ioBit);
    }

    protected int getStaticInputRegister(int ioBit) {
        return inputRegisterId(getIOSide(), ioBit);
    }

    //region GateTile overrides
    @Override
    protected boolean canRotate() {
        return false;
    }
    //endregion

    //region IIOConnectionTile overrides
    @Override
    public boolean isInputIOMode() {
        return getShape() == 0;
    }

    @Override
    public int getIOSide() {
        return getRotation();
    }
    //endregion

    //region BaseTile overrides
    @Override
    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) {
        int oldState = getState();
        int newState =  pullInputMask(container) & 0xF | pullOutputMask(container) << 4;
        if (oldState != newState) {
            setState(newState);
            sendStateUpdate();
        }
    }

    protected int pullInputMask(ICSimulationContainer container) {
        return !isInputIOMode() && container.pullRegisterValue(regId) != 0 ? 0x4 : 0;
    }

    protected int pullOutputMask(ICSimulationContainer container) {
        return isInputIOMode() && container.pullRegisterValue(regId) != 0 ? 0x4 : 0;
    }
    //endregion

    //region IGateRenderKey overrides
    @Override
    public int state2() {
        return ioBit & 0xFF;
    }
    //endregion

    //region RedstoneGateTile overrides
    @Override
    protected int redstoneOutputMask() {
        //TODO these are wrong i think?
        return isInputIOMode() ? 0x0 : 0x4;
    }

    @Override
    protected int redstoneInputMask() {
        return isInputIOMode() ? 0x4 : 0x0;
    }
    //endregion

    //region FETile overrides
    @Override
    public void allocate(Allocator allocator) {
        if (isInputIOMode()) { // Input from world, output into simulation
            regId = allocator.allocRegisterID(getStaticInputRegister(ioBit));
        } else { // Input from simulation, output into world
            regId = REG_ZERO; // Will be located, then remapped to target static register
        }
    }

    @Override
    public void locate(IPathFinder pathFinder) {
        if (!isInputIOMode()) {
            int absR = toAbsoluteRotation(2);
            int absDir = IRotatableICTile.rotationToDir(absR);
            PathFinderResult pfr = pathFinder.doPathFinding((d, p) -> d == absDir);
            if (pfr.outputRegisters.size() > 1) {
                getEditor().getStateMachine().getCompilerLog().addProblem(new MultipleDriversError(getPos(), pfr.outputRegisters));
            }
            if (!pfr.outputRegisters.isEmpty()) {
                regId = pfr.outputRegisters.get(0);
            }
        }
    }

    @Override
    public void registerRemaps(RemapRegistry remapRegistry) {
        if (!isInputIOMode() && regId != REG_ZERO) {
            remapRegistry.addRemap(regId, getStaticOutputRegister(ioBit));
        }
    }

    @Override
    public void consumeRemaps(RemapProvider remapProvider) {
        regId = remapProvider.getRemappedRegisterID(regId);
    }

    @Override
    public void collect(Collector collector) {
        // Static registers are pre-added during assembler instantiation
//        if (isInputIOMode()) {
//            collector.addRegister(regId, new ByteRegister());
//        }
    }

    @Override
    public Optional<Integer> getOutputRegister(int outDir, int outPort) {
        int gateOutputDir = IRotatableICTile.rotationToDir(toAbsoluteRotation(2));
        return isInputIOMode() && outDir == gateOutputDir ? Optional.of(regId) : Optional.empty();
    }

    @Override
    public Optional<Integer> getInputRegister(int inDir, int inPort) {
        int gateInputDir = IRotatableICTile.rotationToDir(toAbsoluteRotation(2));
        return !isInputIOMode() && inDir == gateInputDir ? Optional.of(regId) : Optional.empty();
    }
    //endregion
}
