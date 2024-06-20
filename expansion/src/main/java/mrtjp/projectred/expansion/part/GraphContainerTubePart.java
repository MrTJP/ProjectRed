package mrtjp.projectred.expansion.part;

import codechicken.multipart.api.part.TickablePart;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.expansion.TubeType;
import mrtjp.projectred.expansion.GraphDebugManager;
import mrtjp.projectred.expansion.graphs.GraphContainer;
import mrtjp.projectred.expansion.graphs.GraphNode;
import net.minecraft.world.level.chunk.LevelChunk;
import org.jetbrains.annotations.Nullable;

public abstract class GraphContainerTubePart extends RedstoneTubePart implements TickablePart, GraphContainer {

    protected final GraphNode node = new GraphNode(this);

    public GraphContainerTubePart(TubeType pipeType) { super(pipeType); }

    //region MultiPart events
    @Override
    public void onAdded() {
        super.onAdded();
        if (!level().isClientSide) {
            node.onAdded();
        }
    }

    @Override
    public void onRemoved() {
        if (!level().isClientSide) {
            int sideMask = node.onRemoved();
            // If nodes notified on multiple sides, a link was severed.
            if (Integer.bitCount(sideMask) > 1) {
                onRemovalSeveredLink();
            }
        }
        // Notify neighbors afterward
        super.onRemoved();
    }

    @Override
    public void onChunkLoad(LevelChunk chunk) {
        super.onChunkLoad(chunk);
        if (!level().isClientSide) {
            // We will rebuild on next tick
            node.markLinksChanged();
        }
    }

    @Override
    public void onWorldJoin() {
        super.onWorldJoin();
        if (level().isClientSide()) {
            GraphDebugManager.getInstance(level()).onWorldJoin(this);
        }
    }

    @Override
    public void onWorldSeparate() {
        super.onWorldSeparate();
        if (level().isClientSide()) {
            GraphDebugManager.getInstance(level()).onWorldSeparate(this);
        }
    }

    @Override
    public void maskChangeEvent(boolean internalChange, boolean externalChange) {
        super.maskChangeEvent(internalChange, externalChange);
        if (internalChange || externalChange) {
            node.markLinksChanged();
        }
    }
    //endregion

    @Override
    public void tick() {
        if (!level().isClientSide()) {
            node.onTick();
        }
    }

    protected void onRemovalSeveredLink() {
    }

    //region IGraphNodeContainer overrides
    @Override
    public GraphNode getNode() {
        return node;
    }

    @Override
    public void onNodeChanged(boolean linksChanged, boolean stateChange) {

    }

    @Override
    public boolean canPropagate(int dir) {
        return maskConnects(dir);
    }

    @Nullable
    @Override
    public GraphContainer getNodeTowards(int dir) {
        if (!maskConnects(dir)) return null;

        CenterLookup lookup = CenterLookup.lookupStraightCenter(level(), pos(), dir);
        if (lookup.part instanceof GraphContainer c) {
            return c;
        } else if (lookup.tile instanceof GraphContainer c) {
            return c;
        }

        return null;
    }
    //endregion
}
