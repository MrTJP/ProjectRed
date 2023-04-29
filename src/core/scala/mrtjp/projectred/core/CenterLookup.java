package mrtjp.projectred.core;

import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.block.TileMultiPart;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class CenterLookup {

    //Starting conditions
    public final World world;
    public final BlockPos pos;
    public final int direction;

    // Located objects
    public final BlockState state;
    public final Block block;
    public final TileEntity tile;
    public final TMultiPart part;

    // Conditions for reverse lookup
    public final BlockPos otherPos;
    public final int otherDirection;

    public CenterLookup(World world, BlockPos pos, int direction, BlockState state, TileEntity tile, TMultiPart part, BlockPos otherPos, int otherDirection) {
        this.world = world;
        this.pos = pos;
        this.direction = direction;
        this.state = state;
        this.block = state.getBlock();
        this.tile = tile;
        this.part = part;
        this.otherPos = otherPos;
        this.otherDirection = otherDirection;
    }

    public static CenterLookup lookupInsideFace(World world, BlockPos pos, int direction) {

        int otherDir = direction ^ 1;

        BlockState state = world.getBlockState(pos);
        TileEntity tile = world.getBlockEntity(pos);
        TMultiPart part = null;
        if (tile instanceof TileMultiPart) {
            part = ((TileMultiPart) tile).getSlottedPart(direction);
        }

        return new CenterLookup(world, pos, direction, state, tile, part, pos, otherDir);
    }

    public static CenterLookup lookupStraightCenter(World world, BlockPos pos, int direction) {

        BlockPos otherPos = pos.relative(Direction.values()[direction]);
        int otherDir = direction ^ 1;

        BlockState state = world.getBlockState(otherPos);
        TileEntity tile = world.getBlockEntity(otherPos);
        TMultiPart part = null;
        if (tile instanceof TileMultiPart) {
            part = ((TileMultiPart) tile).getSlottedPart(6);
        }

        return new CenterLookup(world, pos, direction, state, tile, part, otherPos, otherDir);
    }
}
