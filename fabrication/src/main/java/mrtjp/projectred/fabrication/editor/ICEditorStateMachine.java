package mrtjp.projectred.fabrication.editor;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.projectred.fabrication.engine.BaseTile;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.IIOConnectionTile;
import mrtjp.projectred.fabrication.engine.PRFabricationEngine;
import mrtjp.projectred.fabrication.engine.log.ICCompilerLog;
import mrtjp.projectred.fabrication.engine.log.IODirectionMismatchError;
import mrtjp.projectred.fabrication.engine.log.NoInputsError;
import mrtjp.projectred.fabrication.engine.log.NoOutputsError;
import net.minecraft.nbt.CompoundTag;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.*;

public class ICEditorStateMachine {

    public static final int KEY_STATE_CHANGED = 0;
    public static final int KEY_COMPILER_LOG_CLEARED = 1;
    public static final int KEY_COMPILER_LOG_NODE_ADDED = 2;
    public static final int KEY_COMPILER_LOG_NODE_EXECUTED = 3;
    public static final int KEY_COMPILER_LOG_PROBLEM_ADDED = 4;

    public static final int KEY_CLIENT_COMPILE_CLICKED = 10;

    private final ICWorkbenchEditor editor;

    private static final int STATE_INITIAL = 0;
    private static final int STATE_AWAITING_COMPILE = 1;
    private static final int STATE_COMPILING = 2;
    private static final int STATE_SIMULATING = 3;
    private static final int STATE_COMPILE_FAILED = 4;

    private final State[] states = {
            new StateInitial(),
            new StateAwaitingCompile(),
            new StateCompiling(),
            new StateSimulating(),
            new CompileFailed(),
    };

    private int currentState = STATE_INITIAL;

    private final @Nullable StateMachineCallback callback;

    private final ICSimulationContainer simulationContainer = new ICSimulationContainer();
    private final ICCompilerLog compilerLog = new ICCompilerLog(this);

    private String lastCompiledFlatMap = PRFabricationEngine.EMPTY_FLAT_MAP_SERIALIZED;

    private boolean autoCompileOnChange = false; //TODO client-side toggle

    public ICEditorStateMachine(ICWorkbenchEditor editor, @Nullable StateMachineCallback callback) {
        this.editor = editor;
        this.callback = callback;
    }

    public ICCompilerLog getCompilerLog() {
        return compilerLog;
    }

    public void save(CompoundTag tag) {
        tag.putByte(KEY_COMP_STATE, (byte) currentState);
        tag.putString(KEY_FLAT_MAP, lastCompiledFlatMap);

        CompoundTag simTag = new CompoundTag();
        simulationContainer.save(simTag);
        tag.put(KEY_SIMULATION, simTag);

        CompoundTag logTag = new CompoundTag();
        compilerLog.save(logTag);
        tag.put(KEY_COMPILER_LOG, logTag);
    }

    public void load(CompoundTag tag) {
        currentState = tag.getByte(KEY_COMP_STATE) & 0xFF;
        lastCompiledFlatMap = tag.getString(KEY_FLAT_MAP);
        simulationContainer.load(tag.getCompound(KEY_SIMULATION));
        compilerLog.load(tag.getCompound(KEY_COMPILER_LOG));
    }

    public void writeDesc(MCDataOutput out) {
        out.writeByte(currentState);
        simulationContainer.writeDesc(out);
        compilerLog.writeDesc(out);
    }

    public void readDesc(MCDataInput in) {
        currentState = in.readUByte();
        simulationContainer.readDesc(in);
        compilerLog.readDesc(in);
    }

    public void reset() {
        enterState(STATE_INITIAL, true);
    }

    public void readStateMachineStream(MCDataInput in, int key) {
        switch (key) {
            case KEY_STATE_CHANGED:
                enterStateOnClient(in.readUByte());
                break;
            case KEY_COMPILER_LOG_CLEARED:
            case KEY_COMPILER_LOG_NODE_ADDED:
            case KEY_COMPILER_LOG_NODE_EXECUTED:
            case KEY_COMPILER_LOG_PROBLEM_ADDED:
                compilerLog.readLogStream(in, key);
                break;
            case KEY_CLIENT_COMPILE_CLICKED:
                onCompileTriggered();
                break;
            default:
                throw new IllegalArgumentException("Unknown compiler stream key: " + key);
        }
    }

    public MCDataOutput getStateMachineStream(int key) {
        return editor.getStateMachineStream(key);
    }

    //region State Machine events
    public void onTick(long time) {
        states[currentState].onTick(time);
    }

    public void onTileMapChanged() {
        states[currentState].onTileMapChanged();
    }

    public void onCompileTriggered() {
        states[currentState].onCompileTriggered();
    }

    public void onInputRegistersChanged(int rotation, Function<Short, Short> changeFunction) {
        states[currentState].onInputRegistersChanged(rotation, changeFunction);
    }
    //endregion

    //region Client-side utilities
    public void sendCompileButtonClicked() {
        // Notifies server to call onCompileTriggered
        getStateMachineStream(KEY_CLIENT_COMPILE_CLICKED);
    }
    public boolean canTriggerCompile() {
        return states[currentState].canTransitionTo(STATE_COMPILING);
    }
    public boolean isCompiling() {
        return currentState == STATE_COMPILING;
    }
    public boolean isSimulating() {
        return currentState == STATE_SIMULATING;
    }
    public boolean didLastCompileFailed() {
        return getCompilerLog().getErrorCount() > 0;
    }
    //endregion

    private void enterState(int id, boolean force) {
        if (currentState == id) return;

        if (!force && !states[currentState].canTransitionTo(id))
            throw new RuntimeException("Illegal state change requested");

        int oldState = currentState;

        states[currentState].onStateLeaving(id);
        currentState = id;
        states[currentState].onStateEntered(oldState);

        LOGGER.info("State transition: " + oldState + " -> " + currentState);
        editor.markDirty();
    }

    private void enterStateAndSend(int id) {
        // Shift state
        enterState(id, false);

        // Notify clients to also shift states
        getStateMachineStream(KEY_STATE_CHANGED).writeByte(currentState);
    }

    private void enterStateOnClient(int id) {
        int oldState = currentState;

        states[currentState].onClientStateLeaving(id);
        currentState = id;
        states[currentState].onClientStateEntered(oldState);

        LOGGER.info("Client state transition: " + oldState + " -> " + currentState);
    }

    public interface StateMachineCallback {

        void onCompileStart();

        void onCompileComplete();

        void onCompileFailed();

        void onSimulationComplete(int changeMask, ICSimulationContainer container);
    }

    private interface State {

        //region Server-side events
        default void onTick(long time) { }

        default void onTileMapChanged() { }

        default void onCompileTriggered() { }

        default void onInputRegistersChanged(int rotation, Function<Short, Short> changeFunction) { }

        boolean canTransitionTo(int id);

        default void onStateEntered(int previousStateId) { }

        default void onStateLeaving(int nextStateId) { }
        //endregion

        //region Client-side events
        default void onClientStateEntered(int previousStateId) { }

        default void onClientStateLeaving(int nextStateId) { }
        //endregion
    }

    private class StateInitial implements State {

        @Override
        public void onTileMapChanged() {
            enterStateAndSend(STATE_AWAITING_COMPILE);
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_AWAITING_COMPILE;
        }

        @Override
        public void onStateEntered(int previousStateId) {
            compilerLog.clearAndSend();
        }
    }

    private class StateAwaitingCompile implements State {

        @Override
        public void onTick(long time) {
            if (autoCompileOnChange) {
                enterStateAndSend(STATE_COMPILING);
            }
        }

        @Override
        public void onCompileTriggered() {
            enterStateAndSend(STATE_COMPILING);
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_COMPILING;
        }
    }

    private class StateCompiling implements State {

        private @Nullable ICStepThroughAssembler assembler = null;

        @Override
        public void onTick(long time) {
            if (assembler == null) {
                LOGGER.warn("Compiler assembler is null!");
                restartAssembly();
                return;
            }

            if (!assembler.isDone()) {
                long nanoTime = System.nanoTime();
                long elapsedTime;
                do {
                    assembler.stepIn();
                    elapsedTime = System.nanoTime() - nanoTime;
                } while (elapsedTime < 500000L && !assembler.isDone()); // Use up to 0.5ms to compile
            }

            if (assembler.isDone()) {
                ICFlatMap map = assembler.result();
                assembler = null; //TODO make assemblers clearable
                lastCompiledFlatMap = PRFabricationEngine.instance.serializeFlatMap(map);
                simulationContainer.setFlatMap(map);

                if (compilerLog.getErrorCount() > 0) {
                    enterStateAndSend(STATE_COMPILE_FAILED);
                    if (callback != null) callback.onCompileFailed();
                } else {
                    enterStateAndSend(STATE_SIMULATING);
                    if (callback != null) callback.onCompileComplete();
                }
            }
        }

        @Override
        public void onTileMapChanged() {
            restartAssembly();
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_SIMULATING || id == STATE_COMPILE_FAILED;
        }

        @Override
        public void onStateEntered(int previousStateId) {
            restartAssembly();
        }

        @Override
        public void onStateLeaving(int nextStateId) {
            assembler = null;
        }

        private void restartAssembly() {
            assembler = PRFabricationEngine.instance.newStepThroughAssembler();
            assembler.setEventReceiver(compilerLog);
            compilerLog.clearAndSend();

            assembler.addTileMap(editor.getTileMap(), Collections.emptyMap());
            if (callback != null) callback.onCompileStart();

            // Check for problems detectable before compilation

            int ioMask = 0; // 2 bits per side, 0x1 = input, 0x2 = output, lookup with 0x3 << side*2
            Collection<IIOConnectionTile> ioTiles = editor.getTileMap().getIOTiles();
            for (var io : ioTiles) {
                int s = io.getIOSide();
                int m = io.isInputIOMode() ? 0x1 : 0x2;
                ioMask |= m << s*2;
            }

            // Check for sides marked as both input and output
            for (int r = 0; r < 4; r++) {
                int m = ioMask >> r*2 & 0x3;
                if (m == 0x3) {
                    int finalR = r;
                    List<TileCoord> coordList = ioTiles.stream()
                            .filter(io -> io.getIOSide() == finalR)
                            .map(io -> ((BaseTile)io).getPos())
                            .toList();

                    compilerLog.addProblem(new IODirectionMismatchError(coordList));
                }
            }

            if ((ioMask & 0x55) == 0) {
                compilerLog.addProblem(new NoInputsError());
            }

            if ((ioMask & 0xAA) == 0) {
                compilerLog.addProblem(new NoOutputsError());
            }
        }
    }

    private class StateSimulating implements State {

        private long lastTime = -1;

        @Override
        public void onTick(long time) {

            // Note: Because this is always -1 initially, 1 tick will be missed on every chunk load. Not a big deal for
            //       IC Workbench tile, because there are no external interaction. But FabricatedGateParts get around this
            //       by saving the elapsed time to NBT, and then use IChunkLoadTile hook to convert the elapsed to new lastTime.
            //       May be worth implementing IChunkLoadTile on workbench tile, but this is okay for now.
            if (lastTime == -1) {
                lastTime = time;
                return;
            }

            long elapsedTime = time - lastTime;
            lastTime = time;

            simulationContainer.progressTime(elapsedTime);
            simulationContainer.pushTime();
            propagateAndNotify();
        }

        @Override
        public void onInputRegistersChanged(int rotation, Function<Short, Short> changeFunction) {
            short oldInput = simulationContainer.getInput(rotation);
            short newInput = changeFunction.apply(oldInput);
            LOGGER.info("oldInput: " + oldInput + ", newInput: " + newInput);
            if (oldInput != newInput) {
                simulationContainer.setInput(rotation, newInput);
                simulationContainer.pushInputs(1 << rotation);
                propagateAndNotify();
            }
        }

        private void propagateAndNotify() {
            simulationContainer.simulate();
            int changeMask = simulationContainer.pullOutputs();
            if (callback != null) callback.onSimulationComplete(changeMask, simulationContainer);
        }

        @Override
        public void onTileMapChanged() {
            enterStateAndSend(STATE_AWAITING_COMPILE); //TODO enter initial if tile map was emptied
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_AWAITING_COMPILE;
        }

        @Override
        public void onStateEntered(int previousStateId) {
            lastTime = -1;
        }
    }

    private class CompileFailed implements State {

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_AWAITING_COMPILE;
        }

        @Override
        public void onTileMapChanged() {
            enterStateAndSend(STATE_AWAITING_COMPILE);
        }
    }
}
