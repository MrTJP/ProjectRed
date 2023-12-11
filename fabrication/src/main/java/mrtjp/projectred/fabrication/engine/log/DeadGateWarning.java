package mrtjp.projectred.fabrication.engine.log;

import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import net.minecraft.network.chat.Component;

import java.util.List;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_DEAD_GATE_DESC;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_DEAD_GATE_TITLE;

public class DeadGateWarning extends SimpleLocatableProblem {

    public DeadGateWarning() {
        super(CompileProblemType.DEAD_GATE, CompileProblemSeverity.WARNING);
    }

    public DeadGateWarning(TileCoord coord) {
        super(CompileProblemType.DEAD_GATE, CompileProblemSeverity.WARNING, coord);
    }

    @Override
    public Component getName() {
        return Component.translatable(UL_DEAD_GATE_TITLE);
    }

    @Override
    public void buildToolTip(List<Component> tooltip) {
        tooltip.add(Component.translatable(UL_DEAD_GATE_DESC).withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
    }
}
