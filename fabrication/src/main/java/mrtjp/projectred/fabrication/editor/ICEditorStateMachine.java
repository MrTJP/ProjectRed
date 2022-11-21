package mrtjp.projectred.fabrication.editor;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.projectred.fabrication.engine.ICCompilerLog;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.PRFabricationEngine;
import net.minecraft.nbt.CompoundTag;

import java.util.Collections;
import java.util.function.Function;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;

public class ICEditorStateMachine {

    public static final int KEY_STATE_CHANGED = 0;
    public static final int KEY_COMPILER_LOG_NODE_ADDED = 1;
    public static final int KEY_COMPILER_LOG_NODE_EXECUTED = 2;

    public static final int KEY_CLIENT_COMPILE_CLICKED = 10;

    private final ICWorkbenchEditor editor;

    private static final int STATE_INITIAL = 0;
    private static final int STATE_AWAITING_COMPILE = 1;
    private static final int STATE_COMPILING = 2;
    private static final int STATE_SIMULATING = 3;

    private final State[] states = {
            new StateInitial(),
            new StateAwaitingCompile(),
            new StateCompiling(),
            new StateSimulating()
    };

    private int currentState = STATE_INITIAL;

    private final StateMachineCallback callback;

    private final ICSimulationContainer simulationContainer = new ICSimulationContainer();
    private final ICCompilerLog compilerLog = new ICCompilerLog(this);

    private boolean autoCompileOnChange = false; //TODO client-side toggle

    public ICEditorStateMachine(ICWorkbenchEditor editor, StateMachineCallback callback) {
        this.editor = editor;
        this.callback = callback;
    }

    public ICCompilerLog getCompilerLog() {
        return compilerLog;
    }

    public void save(CompoundTag tag) {
        tag.putByte("comp_state", (byte) currentState);
        simulationContainer.save(tag);
        compilerLog.save(tag);
    }

    public void load(CompoundTag tag) {
        currentState = tag.getByte("comp_state") & 0xFF;
        simulationContainer.load(tag);
        compilerLog.load(tag);
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
            case KEY_COMPILER_LOG_NODE_ADDED:
            case KEY_COMPILER_LOG_NODE_EXECUTED:
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
            compilerLog.clear();
        }

        @Override
        public void onClientStateEntered(int previousStateId) {
            compilerLog.clear();
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

        private ICStepThroughAssembler assembler = null;

        @Override
        public void onTick(long time) {
            if (assembler == null) {
                LOGGER.warn("Compiler assembler is null!");
                restartAssembly();
                return;
            }

            if (!assembler.isDone()) {
                assembler.stepIn();
            }

            if (assembler.isDone()) {
                ICFlatMap map = assembler.result();
                assembler = null; //TODO make assemblers clearable
                simulationContainer.setFlatMap(map);
                enterStateAndSend(STATE_SIMULATING);
                if (callback != null) callback.onCompileComplete();
            }
        }

        @Override
        public void onTileMapChanged() {
            restartAssembly();
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_SIMULATING;
        }

        @Override
        public void onStateEntered(int previousStateId) {
            restartAssembly();
        }

        @Override
        public void onStateLeaving(int nextStateId) {
            assembler = null;
        }

        @Override
        public void onClientStateEntered(int previousStateId) {
            compilerLog.clear();
        }

        private void restartAssembly() {
            assembler = PRFabricationEngine.instance.newStepThroughAssembler();
            assembler.setEventReceiver(compilerLog);
            compilerLog.clear();

            assembler.addTileMap(editor.getTileMap(), Collections.emptyMap());
            if (callback != null) callback.onCompileStart();
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
}
