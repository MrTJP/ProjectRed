package mrtjp.projectred.transmission;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.BlockMultipart;
import codechicken.multipart.block.TileMultipart;
import codechicken.multipart.util.PartMap;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;

import javax.annotation.Nullable;

public class ConnectionsLib {

    //region Neighbor part lookup
    public static @Nullable MultiPart getFaceCornerPart(Level world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        BlockPos pos2 = pos
                .relative(Direction.values()[absDir])
                .relative(Direction.values()[side]);
        return BlockMultipart.getPart(world, pos2, absDir ^ 1);
    }

    public static @Nullable MultiPart getFaceStraightPart(Level world, BlockPos pos, int side, int r) {
        BlockPos pos2 = pos.relative(Direction.values()[r]);
        return BlockMultipart.getPart(world, pos2, side);
    }

    public static @Nullable MultiPart getFaceInternalPart(Level world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        return BlockMultipart.getPart(world, pos, absDir);
    }

    public static @Nullable MultiPart getCenterPart(Level world, BlockPos pos) {
        return BlockMultipart.getPart(world, pos, 6);
    }

    //region Internal multipart lookup
    public static @Nullable MultiPart getFaceInternalPart(TileMultipart tile, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        return tile.getSlottedPart(absDir);
    }

    public static @Nullable MultiPart getCenterPart(TileMultipart tile) {
        return tile.getSlottedPart(6);
    }
    //endregion
    //endregion

    //region Connectable lookups
    public static @Nullable IConnectable getFaceCornerConnectable(Level world, BlockPos pos, int side, int r) {
        MultiPart part = getFaceCornerPart(world, pos, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static @Nullable IConnectable getFaceStraightConnectable(Level world, BlockPos pos, int side, int r) {
        MultiPart part = getFaceStraightPart(world, pos, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static @Nullable IConnectable getFaceInternalConnectable(Level world, BlockPos pos, int side, int r) {
        MultiPart part = getFaceInternalPart(world, pos, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static @Nullable IConnectable getFaceInternalConnectable(TileMultipart tile, int side, int r) {
        MultiPart part = getFaceInternalPart(tile, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static @Nullable IConnectable getCenterConnectable(Level world, BlockPos pos) {
        MultiPart part = getCenterPart(world, pos);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    //region Internal multipart lookup
    public static @Nullable IConnectable getCenterConnectable(TileMultipart tile) {
        MultiPart part = getCenterPart(tile);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static @Nullable IConnectable getCenterConnectable(BlockEntity tile) {
        if (tile instanceof TileMultipart) {
            return getCenterConnectable((TileMultipart) tile);
        }
        return null;
    }
    //endregion
    //endregion

    //region Clearance checks
    public static boolean outsideCornerEdgeOpen(Level world, BlockPos pos, int side, int r) {
        int dir = Rotation.rotateSide(side, r);
        BlockPos straightPos = pos.relative(Direction.values()[dir]);
        if (world.isEmptyBlock(straightPos)) return true;

        int perpSide = dir ^ 1;

        TileMultipart t = BlockMultipart.getTile(world, straightPos);
        if (t == null) return false; // Non-multipart block is here. We cant go through it.

        // Tile may have parts, but at least all slots that take up this edge must be empty.
        return t.getSlottedPart(perpSide) == null && t.getSlottedPart(side) == null &&
                t.getSlottedPart(PartMap.edgeBetween(perpSide, side)) == null;
    }

    public static boolean insideCornerEdgeOpen(Level world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        return BlockMultipart.getPart(world, pos, PartMap.edgeBetween(absDir, side)) == null;
    }
    //endregion

    //region Connection helpers
    public static boolean connectFaceCorner(IConnectable source, int side, int r, @Nullable IConnectable target) {
        if (target != null) {
            int dir = Rotation.rotateSide(side, r);
            int otherRotation = Rotation.rotationTo(dir ^ 1, side ^ 1);
            int otherSide = dir ^ 1;
            return target.connectCorner(source, otherRotation, otherSide);
        }
        return false;
    }

    public static boolean connectFaceStraight(IConnectable source, int side, int r, @Nullable IConnectable target) {
        if (target != null) {
            int otherRotation = (r + 2) % 4;
            int otherSide = side;
            return target.connectStraight(source, otherRotation, otherSide);
        }
        return false;
    }

    public static boolean connectFaceInternal(IConnectable source, int side, int r, @Nullable IConnectable target) {
        if (target != null) {
            int dir = Rotation.rotateSide(side, r);
            int otherRotation = Rotation.rotationTo(dir, side);
            return target.connectInternal(source, otherRotation);
        }
        return false;
    }
    //endregion

}
