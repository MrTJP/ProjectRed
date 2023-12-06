package mrtjp.projectred.core;

import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.TileMultipart;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;

import javax.annotation.Nullable;

public class CenterLookup {

    //Starting conditions
    public final Level world;
    public final BlockPos pos;
    public final int direction;

    // Located objects
    public final BlockState state;
    public final Block block;
    public final @Nullable BlockEntity tile;
    public final @Nullable MultiPart part;

    // Conditions for reverse lookup
    public final BlockPos otherPos;
    public final int otherDirection;

    public CenterLookup(Level world, BlockPos pos, int direction, BlockState state, @Nullable BlockEntity tile, @Nullable MultiPart part, BlockPos otherPos, int otherDirection) {
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

    public static CenterLookup lookupInsideFace(Level world, BlockPos pos, int direction) {

        int otherDir = direction ^ 1;

        BlockState state = world.getBlockState(pos);
        BlockEntity tile = world.getBlockEntity(pos);
        MultiPart part = null;
        if (tile instanceof TileMultipart) {
            part = ((TileMultipart) tile).getSlottedPart(direction);
        }

        return new CenterLookup(world, pos, direction, state, tile, part, pos, otherDir);
    }

    public static CenterLookup lookupStraightCenter(Level world, BlockPos pos, int direction) {

        BlockPos otherPos = pos.relative(Direction.values()[direction]);
        int otherDir = direction ^ 1;

        BlockState state = world.getBlockState(otherPos);
        BlockEntity tile = world.getBlockEntity(otherPos);
        MultiPart part = null;
        if (tile instanceof TileMultipart) {
            part = ((TileMultipart) tile).getSlottedPart(6);
        }

        return new CenterLookup(world, pos, direction, state, tile, part, otherPos, otherDir);
    }
}
