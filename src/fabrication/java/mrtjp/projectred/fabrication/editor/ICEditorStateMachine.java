package mrtjp.projectred.fabrication.editor;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.api.ICFlatMap;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.projectred.fabrication.engine.ICCompilerLog;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.PRFabricationEngine;
import net.minecraft.nbt.CompoundNBT;

import java.util.Collections;
import java.util.function.Function;

import static mrtjp.projectred.ProjectRedFabrication.LOGGER;

public class ICEditorStateMachine {

    public static final int KEY_STATE_CHANGED = 0;
    public static final int KEY_COMPILER_LOG_ADDED = 1;
    public static final int KEY_COMPILER_LOG_EXEC = 2;

    private final ICWorkbenchEditor editor;

    private static final int STATE_INITIAL = 0;
    private static final int STATE_COMPILING = 1;
    private static final int STATE_READY = 2;

    private final State[] states = {
            new StateInitial(),
            new StateCompiling(),
            new StateReady()
    };

    private int currentState = STATE_INITIAL;

    private final StateMachineCallback callback;

    private final ICSimulationContainer simulationContainer = new ICSimulationContainer();
    private final ICCompilerLog compilerLog = new ICCompilerLog(this);

    public ICEditorStateMachine(ICWorkbenchEditor editor, StateMachineCallback callback) {
        this.editor = editor;
        this.callback = callback;
    }

    public void save(CompoundNBT tag) {
        tag.putByte("comp_state", (byte) currentState);
        simulationContainer.save(tag);
    }

    public void load(CompoundNBT tag) {
        currentState = tag.getByte("comp_state") & 0xFF;
        simulationContainer.load(tag);
    }

    public void writeDesc(MCDataOutput out) {
        out.writeByte(currentState);
        simulationContainer.writeDesc(out);
    }

    public void readDesc(MCDataInput in) {
        currentState = in.readUByte();
        simulationContainer.readDesc(in);
    }

    public void readCompilerStream(MCDataInput in, int key) {
        switch (key) {
            case KEY_STATE_CHANGED:
                currentState = in.readUByte();
                LOGGER.info("CLIENT READ STATE: " + currentState);
                break;
            case KEY_COMPILER_LOG_ADDED:
            case KEY_COMPILER_LOG_EXEC:
                compilerLog.readLogStream(in, key);
                break;
            default:
                throw new IllegalArgumentException("Unknown compiler stream key: " + key);
        }
    }

    public MCDataOutput getCompilerStream(int key) {
        return editor.getCompilerStream(key);
    }

    public void onTick(long time) {
        states[currentState].onTick(time);
    }

    public void onTileMapChanged() {
        states[currentState].onTileMapChanged();
    }

    public void onInputRegistersChanged(int rotation, Function<Short, Short> changeFunction) {
        states[currentState].onInputRegistersChanged(rotation, changeFunction);
    }

    private boolean enterState(int id) {
        if (currentState == id) return false;

        if (!states[currentState].canTransitionTo(id)) return false;

        int oldState = currentState;

        states[currentState].onStateLeaving(id);
        currentState = id;
        states[currentState].onStateEntered(oldState);

        LOGGER.info("State transition: " + oldState + " -> " + currentState);
        getCompilerStream(KEY_STATE_CHANGED).writeByte(currentState);

        return true;
    }

    public interface StateMachineCallback {

        void onCompileStart();

        void onCompileComplete();

        void onSimulationComplete(int changeMask, ICSimulationContainer container);
    }

    private interface State {

        default void onTick(long time) { }

        default void onTileMapChanged() { }

        default void onInputRegistersChanged(int rotation, Function<Short, Short> changeFunction) { }

        boolean canTransitionTo(int id);

        void onStateEntered(int previousStateId);

        void onStateLeaving(int nextStateId);
    }

    private class StateInitial implements State {

        @Override
        public void onTileMapChanged() {
            enterState(STATE_COMPILING);
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_COMPILING;
        }

        @Override
        public void onStateEntered(int previousStateId) { }

        @Override
        public void onStateLeaving(int nextStateId) { }
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
                enterState(STATE_READY);
                if (callback != null) callback.onCompileComplete();
            }
        }

        @Override
        public void onTileMapChanged() {
            restartAssembly();
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_READY;
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
            compilerLog.clearLog();

            assembler.addTileMap(editor.getTileMap(), Collections.emptyMap());
            if (callback != null) callback.onCompileStart();
        }
    }

    private class StateReady implements State {

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
            enterState(STATE_COMPILING);
        }

        @Override
        public boolean canTransitionTo(int id) {
            return id == STATE_COMPILING;
        }

        @Override
        public void onStateEntered(int previousStateId) {
            lastTime = -1;
        }

        @Override
        public void onStateLeaving(int nextStateId) { }
    }
}
