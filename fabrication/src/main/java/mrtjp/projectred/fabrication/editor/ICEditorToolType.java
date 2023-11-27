package mrtjp.projectred.fabrication.editor;

import mrtjp.projectred.fabrication.editor.tools.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public enum ICEditorToolType {
    INTERACT_TOOL(InteractTool::new),
    ERASE_TOOL(EraseTool::new),
    GATE_PLACER_TOOL(GatePlacerTool::new),

    WIRE_PLACER_TOOL(WirePlacerTool::new),

    ;

    private final Supplier<IICEditorTool> factory;

    ICEditorToolType(Supplier<IICEditorTool> factory) {
        this.factory = factory;
    }

    public IICEditorTool createTool() {
        return factory.get();
    }

    public static ArrayList<IICEditorTool> createToolList() {
        return Arrays.stream(ICEditorToolType.values())
                .map(ICEditorToolType::createTool)
                .collect(Collectors.toCollection(ArrayList::new));
    }
}
