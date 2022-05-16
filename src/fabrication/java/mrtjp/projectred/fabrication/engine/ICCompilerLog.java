package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.projectred.fabrication.editor.ICEditorStateMachine;

import static mrtjp.projectred.fabrication.editor.ICEditorStateMachine.KEY_COMPILER_LOG_ADDED;
import static mrtjp.projectred.fabrication.editor.ICEditorStateMachine.KEY_COMPILER_LOG_EXEC;

public class ICCompilerLog implements ICStepThroughAssembler.EventReceiver {

    private final ICEditorStateMachine compiler;

    public ICCompilerLog(ICEditorStateMachine compiler) {
        this.compiler = compiler;
    }

    public void clearLog() {

    }

    @Override
    public void onStepAdded(ICStepThroughAssembler.AssemblerStepDescriptor descriptor) {

    }

    @Override
    public void onStepExecuted(ICStepThroughAssembler.AssemblerStepResult result) {

    }

    public void readLogStream(MCDataInput in, int key) {
        switch (key) {
            case KEY_COMPILER_LOG_ADDED:
                break;
            case KEY_COMPILER_LOG_EXEC:
                break;
        }

    }

    public static class TreeNode {


    }
}
