package mrtjp.projectred.expansion;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.api.BlockMover;
import mrtjp.projectred.api.MovementController;
import mrtjp.projectred.lib.VecLib;
import net.covers1624.quack.collection.FastStream;
import net.covers1624.quack.util.LazyValue;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.SectionPos;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;

import java.util.*;
import java.util.function.Consumer;

import static mrtjp.projectred.api.MovementDescriptor.MovementStatus.*;

/**
 * A single group of moving blocks, created and managed by {@link MovementManager}
 */
public class MovingStructure implements MovingStructureInfo {

    public final int id;

    private final double speed;
    private final int dir;
    private final List<MovingRow> rows;
    private final int totalSize;

    private final LazyValue<HashSet<ChunkPos>> intersectingChunks = new LazyValue<>(this::computeIntersectingChunks);
    private final LazyValue<HashSet<SectionPos>> renderChunks = new LazyValue<>(this::computeRenderChunks);

    private MovementStatus status;
    private double progress;

    private MovingStructure(int id, double speed, int dir, List<MovingRow> rows, MovementStatus status, double progress) {
        this.id = id;
        this.speed = speed;
        this.dir = dir;
        this.rows = rows;
        this.status = status;
        this.progress = progress;
        this.totalSize = FastStream.of(this.rows).intSum(r -> r.size);
    }

    private MovingStructure(int id, double speed, int dir, List<MovingRow> rows) {
        this(id, speed, dir, rows, PENDING_START, 0D);
    }

    public static MovingStructure fromBlockSet(int id, double speed, int dir, Set<BlockPos> blocks) {
        // Split set of blocks into rows going the opposite direction of the move
        Set<List<BlockPos>> rows = VecLib.resolveRows(blocks, dir^1);

        // Create MovingRows
        List<MovingRow> movingRows = new LinkedList<>();
        for (List<BlockPos> row : rows) {
            movingRows.add(new MovingRow(row, dir));
        }

        return new MovingStructure(id, speed, dir, Collections.unmodifiableList(movingRows), PENDING_START, 0);
    }

    //region Network
    public void writeDesc(MCDataOutput output) {
        output.writeShort(id);
        output.writeDouble(speed);
        output.writeByte(dir);
        output.writeShort(rows.size());
        for (MovingRow row : rows) {
            output.writePos(row.pos);
            output.writeShort(row.size);
        }
        output.writeByte(status.ordinal());
        output.writeDouble(progress); //TODO use integers instead
    }

    public static MovingStructure fromDesc(MCDataInput input) {
        int id = input.readUShort();
        double speed = input.readDouble();
        int dir = input.readUByte();
        int size = input.readUShort();
        List<MovingRow> rows = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            rows.add(new MovingRow(input.readPos(), dir, input.readUShort()));
        }
        MovementStatus status = MovementStatus.values()[input.readUByte()];
        double progress = input.readDouble();

        return new MovingStructure(id, speed, dir, rows, status, progress);
    }
    //endregion

    //region Movement description
    @Override
    public MovementStatus getStatus() {
        return status;
    }

    @Override
    public boolean isMoving() {
        return getStatus() == MOVING || getStatus() == PENDING_FINALIZATION;
    }

    @Override
    public double getProgress() {
        return progress;
    }

    @Override
    public int getSize() {
        return totalSize;
    }

    @Override
    public Vector3 getRenderOffset(float partialTicks) {
        double p = Math.min(progress + speed * partialTicks, 1D);
        return Vector3.fromBlockPos(BlockPos.ZERO.relative(Direction.values()[dir])).multiply(p);
    }
    //endregion

    //region Public accessors
    public HashSet<ChunkPos> getChunkSet() {
        return intersectingChunks.get();
    }

    public boolean intersects(ChunkPos pos) {
        return intersectingChunks.get().contains(pos);
    }

    public boolean contains(BlockPos pos) {
        for (MovingRow row : rows) {
            if (row.contains(pos)) return true;
        }
        return false;
    }

    public Iterator<BlockPos> iteratePreMove() {
        return new Iterator<>() {
            private final Iterator<MovingRow> rowIterator = rows.iterator();
            private Iterator<BlockPos> currentIterator = rowIterator.next().iteratePreMove();

            @Override
            public boolean hasNext() {
                return currentIterator.hasNext() || rowIterator.hasNext();
            }

            @Override
            public BlockPos next() {
                if (!currentIterator.hasNext()) {
                    currentIterator = rowIterator.next().iteratePreMove();
                }
                return currentIterator.next();
            }
        };
    }
    //endregion

    //region Structure management
    public void tickProgress(Level level) {

        // Should not be ticking progress otherwise
        assert status == MOVING || status == PENDING_FINALIZATION;

        if (status == MOVING) {
            progress = Math.min(progress + speed, 1D);
            FastStream.of(rows).forEach(r -> r.pushEntities(level, progress));

            if (progress >= 1D) {
                status = PENDING_FINALIZATION;
            }
        }
    }

    public boolean canMove(Level level) {
        for (MovingRow row : rows) {
            if (!row.canMove(level)) return false;
        }
        return true;
    }

    public void beginMove(Level level) {
        assert status == MovementStatus.PENDING_START;
        status = MOVING;

        FastStream.of(rows).forEach(r -> r.onMovementStarted(level));

        if (level.isClientSide) {
            // Force chunk to re-render so rendering of moving block can be suppressed by MovingBlockSuppressorRenderer
            markChunksForRender();
        }
    }

    public void executePreMove(Level level) {
        // Silently moves blocks to new position
        FastStream.of(rows).forEach(r -> r.moveBlocks(level));
    }

    public void executePostMove(Level level) {
        // Completes the movement by alerting the tile itself, etc
        FastStream.of(rows).forEach(r -> r.postMove(level));
        FastStream.of(rows).forEach(r -> r.onMovementFinished(level));

        // Update neighbors
        Set<BlockPos> changes = new HashSet<>();
        FastStream.of(rows).forEach(r -> r.collectNeighborChanges(level, changes));

        for (BlockPos pos : changes) {
            BlockState state = level.getBlockState(pos);
            state.updateNeighbourShapes(level, pos, 0, 0);
            state.updateIndirectNeighbourShapes(level, pos, 0, 0);
            level.neighborChanged(pos, Blocks.AIR, pos); //TODO use better context here
        }

        // Update lighting
        markBlocksForLightUpdate(level);

        // Update chunk rendering
        if (level.isClientSide) {
            markChunksForRender();
        }

        // Mark chunks as changed
        for (ChunkPos p : getChunkSet()) {
            level.getChunk(p.x, p.z).setUnsaved(true);
        }

        //TODO Tick rescheduling
        status = FINISHED;
    }

    public void cancelMove(Level level) {
        // Shouldn't need to do anything. Nothing happens until the animation is finished
        // TODO MovementController notification for this?
        assert status == MOVING || status == PENDING_FINALIZATION;
        status = CANCELLED;
    }

    @OnlyIn(Dist.CLIENT)
    private void markChunksForRender() {
        FastStream.of(renderChunks.get()).forEach(p -> Minecraft.getInstance().levelRenderer.setSectionDirty(p.x(), p.y(), p.z(), true));
    }

    private void markBlocksForLightUpdate(Level level) {
        FastStream.of(rows).forEach(r -> r.forEachAll(p -> level.getLightEngine().checkBlock(p)));
    }
    //endregion

    //region Private utils
    private HashSet<ChunkPos> computeIntersectingChunks() {
        HashSet<ChunkPos> chunks = new HashSet<>();
        FastStream.of(rows).forEach(r -> r.forEachAll(p -> chunks.add(new ChunkPos(p))));
        return chunks;
    }

    private HashSet<SectionPos> computeRenderChunks() {
        HashSet<SectionPos> chunks = new HashSet<>();
        FastStream.of(rows).forEach(r -> r.forEachAll(p -> {
            // Add all neighbors of blocks as well to update culled faces
            for (int s = 0; s < 6; s++) {
                chunks.add(SectionPos.of(p.relative(Direction.values()[s])));
            }
            // Note: no need to add position itself, as it *must* be in one of above chunks
        }));
        return chunks;
    }
    //endregion

    @Override
    public String toString() {
        return "MovingStructure{" +
                "id=" + id +
                ", speed=" + speed +
                ", dir=" + dir +
                ", progress=" + progress +
                ", rows=" + rows +
                '}';
    }

    /**
     * A single contiguous row of moving blocks
     */
    private static final class MovingRow {

        /**
         * Position of the head of the row. Empty before the move and becomes the first
         * block that is moving post-move.
         */
        public final BlockPos pos;
        /**
         * Direction that all blocks are moving towards.
         */
        public final int dir;
        /**
         * Number of total blocks that are moving.
         */
        public final int size;

        private MovingRow(BlockPos pos, int dir, int size) {
            this.pos = pos;
            this.dir = dir;
            this.size = size;
        }

        private MovingRow(List<BlockPos> row, int dir) {
            // Row's head should be the next block towards dir where everything will move,
            // then followed by the rest of the row
            this.pos = row.getFirst().relative(Direction.values()[dir]);
            this.dir = dir;
            this.size = row.size() + 1;
        }

        //region Public accessors
        public boolean contains(BlockPos pos) {
            BlockPos p1 = VecLib.projectDir(this.pos, dir);
            BlockPos p2 = VecLib.projectDir(pos, dir);

            // If projections towards dir plane are not equal, they cannot be on same axis
            if (!p1.equals(p2)) return false;

            // pos is on the same line as this row. Check if its between start and end
            int a1 = VecLib.rejectComponent(this.pos, dir);
            int a2 = VecLib.rejectComponent(this.pos.relative(Direction.values()[dir ^ 1], size - 1), dir);
            int b = VecLib.rejectComponent(pos, dir);

            return Math.min(a1, a2) <= b && b <= Math.max(a1, a2);
        }

        public void collectNeighborChanges(Level level, Set<BlockPos> changes) {
            forEachAll(p -> {
                changes.add(p);
                for (int s = 0; s < 6; s++) {
                    changes.add(p.relative(Direction.values()[s]));
                }
            });
        }
        //endregion

        //region Row management and control

        /**
         * Check if this row can move. This involves checking for blockages, and asking each block's
         * registered {@link MovementController} if the move is allowed.
         *
         * @param level The level
         * @return True if nothing is blocking the movement and all controllers allow the movement
         */
        public boolean canMove(Level level) {
            if (!level.isLoaded(pos)) return false;
            BlockState state = level.getBlockState(pos);
            if (!(state.isAir() || state.canBeReplaced())) return false;

            Iterator<BlockPos> it = iteratePreMove();
            while (it.hasNext()) {
                BlockPos pos = it.next();

                BlockMover mover = MovementRegistry.getMover(level, pos);
                if (!mover.canMove(level, pos)) return false;

                MovementController controller = MovementRegistry.getMovementController(level, pos);
                // Allow hooks to conditionally block movement
                if (controller != null && !controller.isMovable(level, pos, Direction.values()[dir])) return false;
            }

            return true;
        }

        /**
         * Called once movement has begun. This calls each block's {@link MovementController#onMovementStarted(Level, BlockPos, Direction)}
         *
         * @param level The level
         */
        public void onMovementStarted(Level level) {
            //TODO spawn movement blocks

            if (!level.isClientSide) {
                // Notify blocks/BEs conforming to MovementController about move
                forEachPreMove(p -> {
                    MovementController controller = MovementRegistry.getMovementController(level, p);
                    if (controller != null) controller.onMovementStarted(level, p, Direction.values()[dir]);
                });
            }
        }

        public void pushEntities(Level level, double progress) {
            //TODO
        }

        /**
         * Calls each block's {@link BlockMover#move(Level, BlockPos, Direction)} to actually move the block. This
         * phase typically silently re-locates the block or block entity without alerting neighbors.
         *
         * @param level The level
         */
        public void moveBlocks(Level level) {
            forEachPreMove(p -> {
                BlockMover mover = MovementRegistry.getMover(level, p);
                mover.move(level, p, Direction.values()[dir]);
            });
        }

        /**
         * Called after {@link #moveBlocks(Level)} has finished silently relocating all blocks. Calls
         * {@link BlockMover#postMove(Level, BlockPos)} on each block's mover.
         *
         * @param level The level
         */
        public void postMove(Level level) {
            forEachPostMove(p -> {
                BlockMover mover = MovementRegistry.getMover(level, p);
                mover.postMove(level, p);
            });
        }

        /**
         * Calls each block's {@link MovementController#onMovementFinished(Level, BlockPos)}
         *
         * @param level The level
         */
        public void onMovementFinished(Level level) {
            if (!level.isClientSide) {
                forEachPostMove(p -> {
                    MovementController controller = MovementRegistry.getMovementController(level, pos);
                    if (controller != null) controller.onMovementFinished(level, pos);
                });
            }
        }
        //endregion

        //region Iterators
        /**
         * Iterates all blocks in their pre-move positions, starting at head position
         */
        public Iterator<BlockPos> iteratePreMove() {
            return new MovingRow.RowIterator(1, size);
        }

        /**
         * Iterates all post-moved positions, starting at head position
         */
        public Iterator<BlockPos> iteratePostMove() {
            return new MovingRow.RowIterator(0, size - 1);
        }

        /**
         * Iterates all positions, including initially empty head position.
         */
        public Iterator<BlockPos> iterateAll() {
            return new MovingRow.RowIterator(0, size);
        }

        public void forEachPreMove(Consumer<BlockPos> action) {
            var it = iteratePreMove();
            while (it.hasNext()) {
                action.accept(it.next());
            }
        }

        public void forEachPostMove(Consumer<BlockPos> action) {
            var it = iteratePostMove();
            while (it.hasNext()) {
                action.accept(it.next());
            }
        }

        public void forEachAll(Consumer<BlockPos> action) {
            var it = iterateAll();
            while (it.hasNext()) {
                action.accept(it.next());
            }
        }
        //endregion

        @Override
        public String toString() {
            return "MovingRow{" +
                    "pos={" + pos.getX() + ", " + pos.getY() + ", " + pos.getZ() + "}" +
                    ", size=" + size +
                    ", dir=" + dir +
                    "}";
        }

        /**
         * A ranged iterator for going through a portion of blocks in this row
         */
        private class RowIterator implements Iterator<BlockPos> {

            private final int size;
            private final BlockPos.MutableBlockPos mpos = new BlockPos.MutableBlockPos();
            private int i;

            /**
             * @param start Index of starting block
             * @param size Total number of blocks to iterate
             */
            public RowIterator(int start, int size) {
                this.size = size;
                this.i = start;
            }

            @Override
            public boolean hasNext() {
                return i < size;
            }

            @Override
            public BlockPos next() {
                return mpos.set(pos).move(Direction.values()[dir].getOpposite(), i++);
            }
        }
    }
}
