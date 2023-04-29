package mrtjp.projectred.transmission;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.block.BlockMultiPart;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartMap;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class ConnectionsLib {

    //region Neighbor part lookup
    public static TMultiPart getFaceCornerPart(World world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        BlockPos pos2 = pos
                .relative(Direction.values()[absDir])
                .relative(Direction.values()[side]);
        return BlockMultiPart.getPart(world, pos2, absDir ^ 1);
    }

    public static TMultiPart getFaceStraightPart(World world, BlockPos pos, int side, int r) {
        BlockPos pos2 = pos.relative(Direction.values()[r]);
        return BlockMultiPart.getPart(world, pos2, side);
    }

    public static TMultiPart getFaceInternalPart(World world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        return BlockMultiPart.getPart(world, pos, absDir);
    }

    public static TMultiPart getCenterPart(World world, BlockPos pos) {
        return BlockMultiPart.getPart(world, pos, 6);
    }

    //region Internal multipart lookup
    public static TMultiPart getFaceInternalPart(TileMultiPart tile, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        return tile.getSlottedPart(absDir);
    }

    public static TMultiPart getCenterPart(TileMultiPart tile) {
        return tile.getSlottedPart(6);
    }
    //endregion
    //endregion

    //region Connectable lookups
    public static IConnectable getFaceCornerConnectable(World world, BlockPos pos, int side, int r) {
        TMultiPart part = getFaceCornerPart(world, pos, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static IConnectable getFaceStraightConnectable(World world, BlockPos pos, int side, int r) {
        TMultiPart part = getFaceStraightPart(world, pos, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static IConnectable getFaceInternalConnectable(World world, BlockPos pos, int side, int r) {
        TMultiPart part = getFaceInternalPart(world, pos, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static IConnectable getFaceInternalConnectable(TileMultiPart tile, int side, int r) {
        TMultiPart part = getFaceInternalPart(tile, side, r);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static IConnectable getCenterConnectable(World world, BlockPos pos) {
        TMultiPart part = getCenterPart(world, pos);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    //region Internal multipart lookup
    public static IConnectable getCenterConnectable(TileMultiPart tile) {
        TMultiPart part = getCenterPart(tile);
        if (part instanceof IConnectable) {
            return (IConnectable) part;
        }
        return null;
    }

    public static IConnectable getCenterConnectable(TileEntity tile) {
        if (tile instanceof TileMultiPart) {
            return getCenterConnectable((TileMultiPart) tile);
        }
        return null;
    }
    //endregion
    //endregion

    //region Clearance checks
    public static boolean outsideCornerEdgeOpen(World world, BlockPos pos, int side, int r) {
        int dir = Rotation.rotateSide(side, r);
        BlockPos straightPos = pos.relative(Direction.values()[dir]);
        if (world.isEmptyBlock(straightPos)) return true;

        int perpSide = dir ^ 1;

        TileMultiPart t = BlockMultiPart.getTile(world, straightPos);
        if (t == null) return false; // Non-multipart block is here. We cant go through it.

        // Tile may have parts, but at least all slots that take up this edge must be empty.
        return t.getSlottedPart(perpSide) == null && t.getSlottedPart(side) == null &&
                t.getSlottedPart(PartMap.edgeBetween(perpSide, side)) == null;
    }

    public static boolean insideCornerEdgeOpen(World world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        return BlockMultiPart.getPart(world, pos, PartMap.edgeBetween(absDir, side)) == null;
    }
    //endregion

    //region Connection helpers
    public static boolean connectFaceCorner(IConnectable source, int side, int r, IConnectable target) {
        if (target != null) {
            int dir = Rotation.rotateSide(side, r);
            int otherRotation = Rotation.rotationTo(dir ^ 1, side ^ 1);
            int otherSide = dir ^ 1;
            return target.connectCorner(source, otherRotation, otherSide);
        }
        return false;
    }

    public static boolean connectFaceStraight(IConnectable source, int side, int r, IConnectable target) {
        if (target != null) {
            int otherRotation = (r + 2) % 4;
            int otherSide = side;
            return target.connectStraight(source, otherRotation, otherSide);
        }
        return false;
    }

    public static boolean connectFaceInternal(IConnectable source, int side, int r, IConnectable target) {
        if (target != null) {
            int dir = Rotation.rotateSide(side, r);
            int otherRotation = Rotation.rotationTo(dir, side);
            return target.connectInternal(source, otherRotation);
        }
        return false;
    }
    //endregion

}
