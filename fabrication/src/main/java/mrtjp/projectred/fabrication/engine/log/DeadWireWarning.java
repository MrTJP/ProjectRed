package mrtjp.projectred.fabrication.engine.log;

import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;

import java.util.List;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_DEAD_WIRE_DESC;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_DEAD_WIRE_TITLE;

public class DeadWireWarning extends SimpleLocatableProblem {

    public DeadWireWarning() {
        super(CompileProblemType.DEAD_WIRE, CompileProblemSeverity.WARNING);
    }

    public DeadWireWarning(TileCoord coord) {
        super(CompileProblemType.DEAD_WIRE, CompileProblemSeverity.WARNING, coord);
    }

    @Override
    public Component getName() {
        return new TranslatableComponent(UL_DEAD_WIRE_TITLE);
    }

    @Override
    public void buildToolTip(List<Component> tooltip) {
        tooltip.add(new TranslatableComponent(UL_DEAD_WIRE_DESC).withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
    }
}
