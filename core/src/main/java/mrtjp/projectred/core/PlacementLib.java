package mrtjp.projectred.core;

import codechicken.lib.vec.Vector3;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.GameRules;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;

import java.util.LinkedList;
import java.util.List;

public class PlacementLib {

    private static final List<Block> wireWhiteList = new LinkedList<>();
    private static final List<Block> gateWhiteList = new LinkedList<>();

    static {
        wireWhiteList.add(Blocks.GLOWSTONE);
        wireWhiteList.add(Blocks.PISTON);
        wireWhiteList.add(Blocks.STICKY_PISTON);
        wireWhiteList.add(Blocks.PISTON_HEAD);

        gateWhiteList.add(Blocks.GLASS);
    }

    public static boolean canPlaceWireOnSide(Level world, BlockPos pos, Direction side) {
        BlockState state = world.getBlockState(pos);
        if (wireWhiteList.contains(state.getBlock())) {
            return true;
        }
        return state.isFaceSturdy(world, pos, side);
    }

    public static boolean canPlaceGateOnSide(Level world, BlockPos pos, Direction side) {
        if (canPlaceWireOnSide(world, pos, side)) return true;

        BlockState state = world.getBlockState(pos);
        if (gateWhiteList.contains(state.getBlock())) {
            return true;
        }
        return false;
    }

    public static boolean canPlaceLight(Level world, BlockPos pos, Direction side) {
        if (canPlaceWireOnSide(world, pos, side)) return true;
        if (side == Direction.UP) {
            return Block.canSupportCenter(world, pos, side);
        }
        return false;
    }

    public static void dropTowardsPlayer(Level world, BlockPos pos, ItemStack stack, Player player) {

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
