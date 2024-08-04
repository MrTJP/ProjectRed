package mrtjp.projectred.fabrication.engine.log;

import mrtjp.fengine.TileCoord;
import net.minecraft.network.chat.Component;

import java.util.Collection;
import java.util.List;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_GRAY;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_IO_TYPE_MISMATCH_DESC;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_IO_TYPE_MISMATCH_TITLE;

public class IOTypeMismatchError extends MultiPositionProblem {

    public IOTypeMismatchError(Collection<TileCoord> coordList) {
        super(CompileProblemType.IO_TYPE_MISMATCH, CompileProblemSeverity.ERROR, coordList);
    }

    public IOTypeMismatchError() {
        super(CompileProblemType.IO_TYPE_MISMATCH, CompileProblemSeverity.ERROR);
    }

    @Override
    public Component getName() {
        return Component.translatable(UL_IO_TYPE_MISMATCH_TITLE);
    }

    @Override
    public void buildToolTip(List<Component> tooltip) {
        tooltip.add(Component.translatable(UL_IO_TYPE_MISMATCH_DESC).withStyle(UNIFORM_GRAY));
    }
}
