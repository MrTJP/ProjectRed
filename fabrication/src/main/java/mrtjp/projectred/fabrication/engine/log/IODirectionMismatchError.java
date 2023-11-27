package mrtjp.projectred.fabrication.engine.log;

import mrtjp.fengine.TileCoord;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;

import java.util.Collection;
import java.util.List;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_GRAY;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_IO_DIR_MISMATCH_DESC;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_IO_DIR_MISMATCH_TITLE;

public class IODirectionMismatchError extends MultiPositionProblem {

    public IODirectionMismatchError(Collection<TileCoord> coordList) {
        super(CompileProblemType.IO_DIR_MISMATCH, CompileProblemSeverity.ERROR, coordList);
    }

    public IODirectionMismatchError() {
        super(CompileProblemType.IO_DIR_MISMATCH, CompileProblemSeverity.ERROR);
    }

    @Override
    public Component getName() {
        return new TranslatableComponent(UL_IO_DIR_MISMATCH_TITLE);
    }

    @Override
    public void buildToolTip(List<Component> tooltip) {
        tooltip.add(new TranslatableComponent(UL_IO_DIR_MISMATCH_DESC).withStyle(UNIFORM_GRAY));
    }
}
