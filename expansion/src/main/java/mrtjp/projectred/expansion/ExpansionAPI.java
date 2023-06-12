package mrtjp.projectred.expansion;

import mrtjp.projectred.api.*;
import mrtjp.projectred.expansion.client.MovementClientRegistry;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;

import java.util.HashSet;
import java.util.Set;

public class ExpansionAPI implements IExpansionAPI {

    public static final IExpansionAPI INSTANCE = new ExpansionAPI();

    private ExpansionAPI() { }

    @Override
    public void registerBlockMover(Block block, BlockMover mover) {
        MovementRegistry.registerBlockMover(block, mover);
    }

    @Override
    public void registerFrameInteraction(FrameInteraction interaction) {
        MovementRegistry.registerFrameInteraction(interaction);
    }

    @Override
    public void registerBlockEntityRenderCallback(MovingBlockEntityRenderCallback callback) {
        MovementClientRegistry.registerBlockEntityRendererCallback(callback);
    }

    @Override
    public MovementDescriptor beginMove(Level level, int dir, double speed, Set<BlockPos> blocks) {
        return MovementManager.getInstance(level).beginMove(level, blocks, dir, speed);
    }

    @Override
    public Set<BlockPos> getStructure(Level level, BlockPos pos, BlockPos... exclusions) {
        FrameStickResolver resolver = new FrameStickResolver(level, pos, new HashSet<>(Set.of(exclusions)));
        return resolver.resolve();
    }

    @Override
    public boolean isMoving(Level level, BlockPos pos) {
        return MovementManager.getInstance(level).getMovementInfo(pos).isMoving();
    }
}
