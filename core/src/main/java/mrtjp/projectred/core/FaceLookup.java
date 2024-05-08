package mrtjp.projectred.core;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.TileMultipart;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;

import javax.annotation.Nullable;

public class FaceLookup {

    //Starting conditions
    public final Level world;
    public final BlockPos pos;
    public final int side;
    public final int r;

    //Located objects
    public final BlockState state;
    public final Block block;
    public @Nullable final BlockEntity tile;
    public @Nullable final MultiPart part;

    //Conditions for reverse lookup
    public final BlockPos otherPos;
    public final int otherSide;
    public final int otherRotation;

    public FaceLookup(Level world, BlockPos pos, int side, int r, BlockState state, @Nullable BlockEntity tile, @Nullable MultiPart part, BlockPos otherPos, int otherSide, int otherRotation) {
        this.world = world;
        this.pos = pos;
        this.side = side;
        this.r = r;
        this.state = state;
        this.block = state.getBlock();
        this.tile = tile;
        this.part = part;
        this.otherPos = otherPos;
        this.otherSide = otherSide;
        this.otherRotation = otherRotation;
    }

    public static FaceLookup lookupCorner(Level world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        int otherSide = absDir ^ 1;
        int otherRotation = Rotation.rotationTo(absDir ^ 1, side ^ 1);

        BlockPos pos2 = pos
                .relative(Direction.values()[absDir])
                .relative(Direction.values()[side]);

        BlockState state = world.getBlockState(pos2);
        BlockEntity tile = getBlockEntityForState(world, pos2, state);
        MultiPart part = tile instanceof TileMultipart tmp ? tmp.getSlottedPart(otherSide) : null;

        return new FaceLookup(world, pos, side, r, state, tile, part, pos2, otherSide, otherRotation);
    }

    public static FaceLookup lookupStraight(Level world, BlockPos pos, int side, int r) {
        int otherSide = side;
        int otherRotation = (r + 2) % 4;

        BlockPos pos2 = pos.relative(Direction.values()[Rotation.rotateSide(side, r)]);

        BlockState state = world.getBlockState(pos2);
        BlockEntity tile = getBlockEntityForState(world, pos2, state);
        MultiPart part = tile instanceof TileMultipart tmp ? tmp.getSlottedPart(otherSide) : null;

        return new FaceLookup(world, pos, side, r, state, tile, part, pos2, otherSide, otherRotation);
    }

    public static FaceLookup lookupInsideFace(Level world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        int otherSide = absDir;
        int otherRotation = Rotation.rotationTo(absDir, side);

        BlockState state = world.getBlockState(pos);
        BlockEntity tile = getBlockEntityForState(world, pos, state);
        MultiPart part = tile instanceof TileMultipart tmp ? tmp.getSlottedPart(otherSide) : null;

        return new FaceLookup(world, pos, side, r, state, tile, part, pos, otherSide, otherRotation);
    }

    public static FaceLookup lookupInsideCenter(Level world, BlockPos pos, int side) {
        int otherSide = side;
        int otherRotation = -1; // Part is not on face

        BlockState state = world.getBlockState(pos);
        BlockEntity tile = getBlockEntityForState(world, pos, state);
        MultiPart part = tile instanceof TileMultipart tmp ? tmp.getSlottedPart(6) : null;

        return new FaceLookup(world, pos, side, -1, state, tile, part, pos, otherRotation, otherSide);
    }

    /**
     * Conditionally retrieves a tile if block state allows it and if existing tile is still valid
     */
    static @Nullable BlockEntity getBlockEntityForState(Level level, BlockPos pos, BlockState state) {

        if (!state.hasBlockEntity()) return null;

        BlockEntity tile = level.getBlockEntity(pos);
        if (tile == null || tile.isRemoved()) return null;

        // Special handling for recently emptied multipart tiles, as done in BlockMultipart.getTile().
        // This case would be hit if this lookup is a result of a neighbor update caused by a part removal.
        if (tile instanceof TileMultipart tmp && tmp.getPartList().isEmpty()) {
            return null;
        }

        return tile;
    }
}
