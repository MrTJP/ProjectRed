package mrtjp.projectred.expansion;

import mrtjp.projectred.api.Frame;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

public class FrameStickResolver {

    private final Level level;
    private final BlockPos start;
    private final Set<BlockPos> exclusions;

    private final LinkedList<BlockPos> open = new LinkedList<>();
    private final Set<BlockPos> closed = new HashSet<>();

    public FrameStickResolver(Level level, BlockPos start, Set<BlockPos> exclusions) {
        this.level = level;
        this.start = start;
        this.exclusions = exclusions;
    }

    public Set<BlockPos> resolve() {
        open.clear();
        closed.clear();

        // Seed the starting position
        open.addLast(start);

        // Run the loop
        while (!open.isEmpty()) {
            checkNext();
        }

        return closed;
    }

    private void checkNext() {
        BlockPos next = open.removeFirst();

        // Create a set of what 'next' can stick to
        Set<BlockPos> nextSticks = getNextSticks(next);

        // Check each stuck block
        for (BlockPos stick : nextSticks) {
            // Make sure next position was not already considered
            // TODO open contains check is expensive and probably not needed
            if (closed.contains(stick) || exclusions.contains(stick) || open.contains(stick)) continue;

            // Position is unique. Add it to open list
            open.addLast(stick);
        }

        // Add the current position 'next' to the result list
        closed.add(next);
    }

    /**
     * Creates a set of positions that source is currently stuck to.
     * @param source Source position
     * @return Set of positions that source is stuck to
     */
    private Set<BlockPos> getNextSticks(BlockPos source) {

        // Unless source is a frame, nothing can stick to it
        Frame frame = MovementRegistry.getFrame(level, source);
        if (frame == null) return Collections.emptySet();

        // Query all sides of source
        Set<BlockPos> stuckBlocks = new HashSet<>();
        for (int s = 0; s < 6; s++) {
            Direction dir = Direction.values()[s];
            if (frame.canGrab(level, source, dir)) {

                BlockPos stuckBlockPos = source.relative(dir);
                BlockState stuckBlockState = level.getBlockState(stuckBlockPos);

                // Skip uninteresting blocks such as air
                if (level.isEmptyBlock(stuckBlockPos) || stuckBlockState.isAir()) continue;

                // Don't stick to currently moving blocks
                if (MovementManager.getInstance(level).getMovementInfo(stuckBlockPos).isMoving()) continue;

                // If stuck block is a frame, it must also allow the connection
                Frame stuckFrame = MovementRegistry.getFrame(level, stuckBlockPos);
                if (stuckFrame != null && !stuckFrame.canBeGrabbed(level, stuckBlockPos, dir.getOpposite())) {
                    continue;
                }

                // Block can stick. Add it
                stuckBlocks.add(stuckBlockPos);
            }
        }

        // Ask source for additional sticks
        stuckBlocks.addAll(frame.getAdditionalSticks());

        return stuckBlocks;
    }
}
