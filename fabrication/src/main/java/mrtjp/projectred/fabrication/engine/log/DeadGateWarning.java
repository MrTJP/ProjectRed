package mrtjp.projectred.fabrication.engine.log;

import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;

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
        return new TranslatableComponent(UL_DEAD_GATE_TITLE);
    }

    @Override
    public void buildToolTip(List<Component> tooltip) {
        tooltip.add(new TranslatableComponent(UL_DEAD_GATE_DESC).withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
    }
}
