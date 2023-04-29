package mrtjp.projectred.core;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.block.TileMultiPart;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class FaceLookup {

    //Starting conditions
    public final World world;
    public final BlockPos pos;
    public final int side;
    public final int r;

    //Located objects
    public final BlockState state;
    public final Block block;
    public final TileEntity tile;
    public final TMultiPart part;

    //Conditions for reverse lookup
    public final BlockPos otherPos;
    public final int otherSide;
    public final int otherRotation;

    public FaceLookup(World world, BlockPos pos, int side, int r, BlockState state, TileEntity tile, TMultiPart part, BlockPos otherPos, int otherSide, int otherRotation) {
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

    public static FaceLookup lookupCorner(World world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        int otherSide = absDir ^ 1;
        int otherRotation = Rotation.rotationTo(absDir ^ 1, side ^ 1);

        BlockPos pos2 = pos
                .relative(Direction.values()[absDir])
                .relative(Direction.values()[side]);

        BlockState state = world.getBlockState(pos2);
        TileEntity tile = world.getBlockEntity(pos2);
        TMultiPart part = null;
        if (tile instanceof TileMultiPart) {
            part = ((TileMultiPart) tile).getSlottedPart(otherSide);
        }

        return new FaceLookup(world, pos, side, r, state, tile, part, pos2, otherSide, otherRotation);
    }

    public static FaceLookup lookupStraight(World world, BlockPos pos, int side, int r) {
        int otherSide = side;
        int otherRotation = (r + 2) % 4;

        BlockPos pos2 = pos.relative(Direction.values()[Rotation.rotateSide(side, r)]);

        BlockState state = world.getBlockState(pos2);
        TileEntity tile = world.getBlockEntity(pos2);
        TMultiPart part = null;
        if (tile instanceof TileMultiPart) {
            part = ((TileMultiPart) tile).getSlottedPart(otherSide);
        }

        return new FaceLookup(world, pos, side, r, state, tile, part, pos2, otherSide, otherRotation);
    }

    public static FaceLookup lookupInsideFace(World world, BlockPos pos, int side, int r) {
        int absDir = Rotation.rotateSide(side, r);
        int otherSide = absDir;
        int otherRotation = Rotation.rotationTo(absDir, side);

        BlockState state = world.getBlockState(pos);
        TileEntity tile = world.getBlockEntity(pos);
        TMultiPart part = null;
        if (tile instanceof TileMultiPart) {
            part = ((TileMultiPart) tile).getSlottedPart(otherSide);
        }

        return new FaceLookup(world, pos, side, r, state, tile, part, pos, otherSide, otherRotation);
    }

    public static FaceLookup lookupInsideCenter(World world, BlockPos pos, int side) {

        int otherSide = side;
        int otherRotation = -1; // Part is not on face

        BlockState state = world.getBlockState(pos);
        TileEntity tile = world.getBlockEntity(pos);
        TMultiPart part = null;
        if (tile instanceof TileMultiPart) {
            part = ((TileMultiPart) tile).getSlottedPart(6);
        }

        return new FaceLookup(world, pos, side, -1, state, tile, part, pos, otherRotation, otherSide);
    }
}
