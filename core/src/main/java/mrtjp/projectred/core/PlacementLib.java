package mrtjp.projectred.core;

import codechicken.lib.vec.Vector3;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;

import java.util.LinkedList;
import java.util.List;

public class PlacementLib {

    private static List<Block> wireWhiteList = new LinkedList<>();
    private static List<Block> gateWhiteList = new LinkedList<>();

    static {
        wireWhiteList.add(Blocks.GLOWSTONE);
        wireWhiteList.add(Blocks.PISTON);
        wireWhiteList.add(Blocks.STICKY_PISTON);
        wireWhiteList.add(Blocks.PISTON_HEAD);

        gateWhiteList.add(Blocks.GLASS);
    }

    public static boolean canPlaceWireOnSide(World world, BlockPos pos, Direction side) {
        BlockState state = world.getBlockState(pos);
        if (wireWhiteList.contains(state.getBlock())) {
            return true;
        }
        return state.isFaceSturdy(world, pos, side);
    }

    public static boolean canPlaceGateOnSide(World world, BlockPos pos, Direction side) {
        if (canPlaceWireOnSide(world, pos, side)) return true;

        BlockState state = world.getBlockState(pos);
        if (gateWhiteList.contains(state.getBlock())) {
            return true;
        }
        return false;
    }

    public static boolean canPlaceLight(World world, BlockPos pos, Direction side) {
        if (canPlaceWireOnSide(world, pos, side)) return true;
        if (side == Direction.UP) {
            return Block.canSupportCenter(world, pos, side);
        }
        return false;
    }

    public static void dropTowardsPlayer(World world, BlockPos pos, ItemStack stack, PlayerEntity player) {

        if (world.isClientSide || !world.getGameRules().getBoolean(GameRules.RULE_DOBLOCKDROPS)) {
            return;
        }

        Vector3 bpos = Vector3.fromBlockPos(pos);
        Vector3 d = new Vector3(player.position()).subtract(bpos).normalize();
        Vector3 vel = d.copy().multiply(8.0);
        Vector3 tpos = bpos.add(Vector3.CENTER).add(d.copy().multiply(1.25));

        ItemEntity item = new ItemEntity(world, tpos.x, tpos.y, tpos.z, stack);
        vel.multiply(0.02);
        item.setDeltaMovement(vel.x, vel.y, vel.z);
        item.setPickUpDelay(0);
        world.addFreshEntity(item);
    }
}
