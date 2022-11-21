package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import net.minecraft.block.BlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.world.Explosion;
import net.minecraft.world.World;

public interface IBlockEventTile {

    World getBlockLevel();
    BlockPos getBlockPosition();

    default void onBlockPlaced(LivingEntity player, ItemStack item) {}

    default void onBlockStateReplaced(BlockState newState) {}

    default void onBlockRemoved() {}

    default void loadBlockState(BlockState state) { }
    default BlockState storeBlockState(BlockState defaultState) { return defaultState; }

    default VoxelShape getOutlineShape() { return VoxelShapeCache.getShape(Cuboid6.full); }
    default VoxelShape getCollisionShape() { return getOutlineShape(); }
    default VoxelShape getCullingShape() { return getOutlineShape(); }
    default VoxelShape getRayTraceShape() { return getOutlineShape(); }

    default float getExplosionResistance(Entity exploder, Explosion explosion) { return 0; }
    default float getPlayerRelativeBlockHardness(PlayerEntity player) { return 1/30F; }

    default int getLightValue() { return 0; }

//    default List<ItemStack> getDrops() { return Collections.emptyList(); }
//    ItemStack getPickBlock();

    default ActionResultType onBlockActivated(PlayerEntity player, Hand hand, BlockRayTraceResult hit) { return ActionResultType.PASS; }
    default void onBlockClicked(PlayerEntity player) { }

    default void onEntityCollision(Entity entity) { }
    default void onEntityWalk(Entity entity) { }

    default void onNeighborBlockChanged(BlockPos neighborPos) { }
    default void onNeighborTileChanged(BlockPos neighborPos) { }

    default boolean getWeakChanges() { return false; } //True if listening to weak changes

    default boolean canRedstoneConnect(int side) { return false; }
    default int getStrongPower(int side) { return 0; }
    default int getWeakPower(int side) { return 0; }

    void saveToNBT(CompoundNBT tag);
    void loadFromNBT(CompoundNBT tag);

    void writeDesc(MCDataOutput out);
    void readDesc(MCDataInput in);
}
