package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.vec.Vector3;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import net.minecraft.util.text.ITextProperties;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.List;

public abstract class BaseICEditorTool implements IICEditorTool {

    protected ICWorkbenchEditor editor;

    @Override
    public void bindEditor(ICWorkbenchEditor editor) {
        this.editor = editor;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildTooltip(Vector3 mousePosition, boolean isFirstHit, List<ITextProperties> tooltip) {

        if (!isFirstHit) return;

        TileCoord pos = IICEditorTool.toNearestPosition(mousePosition);
        editor.getTileMap().getBaseTile(pos).ifPresent(tile -> tile.buildToolTip(tooltip));
    }

    protected boolean isInBody(TileCoord coord) {
        TileCoord minBounds = editor.getTileMap().getMinBounds();
        TileCoord maxBounds = editor.getTileMap().getMaxBounds();

        return coord.x > minBounds.x && coord.x < maxBounds.x &&
                coord.y > minBounds.y && coord.y < maxBounds.y &&
                coord.z > minBounds.z && coord.z < maxBounds.z;
    }

    protected boolean isOnIOEdge(TileCoord coord) {
        TileCoord minBounds = editor.getTileMap().getMinBounds();
        TileCoord maxBounds = editor.getTileMap().getMaxBounds();

        if (coord.y != 0) return false;

        int edges = 0;
        if (coord.x == minBounds.x) edges++;
        if (coord.x == maxBounds.x) edges++;
        if (coord.z == minBounds.z) edges++;
        if (coord.z == maxBounds.z) edges++;

        return edges == 1;
    }
}
