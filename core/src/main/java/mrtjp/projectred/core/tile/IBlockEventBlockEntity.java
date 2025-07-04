package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.ItemInteractionResult;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.shapes.VoxelShape;

import javax.annotation.Nullable;

public interface IBlockEventBlockEntity {

    Level getBlockLevel();
    BlockPos getBlockPosition();

    default void onBlockPlaced(@Nullable LivingEntity player, ItemStack item) {}

    default void onBlockStateReplaced(BlockState newState) {}

    default void onBlockRemoved() {}

    default BlockState storeBlockState(BlockState defaultState) { return defaultState; }

    default void tick() { }

    default VoxelShape getOutlineShape() { return VoxelShapeCache.getShape(Cuboid6.full); }
    default VoxelShape getCollisionShape() { return getOutlineShape(); }
    default VoxelShape getCullingShape() { return getOutlineShape(); }
    default VoxelShape getRayTraceShape() { return getOutlineShape(); }

    default float getExplosionResistance(Entity exploder, Explosion explosion) { return 0; }
    default float getPlayerRelativeBlockHardness(Player player) { return 1/30F; }

    default int getLightValue() { return 0; }

    default boolean isFireSource(int side) { return false; }

    default InteractionResult useWithoutItem(Player player, BlockHitResult hit) {
        // And by default, we will attempt to figure out the hand and pass to onBlockActivated
        return InteractionResult.PASS;
    }

    default ItemInteractionResult useItemOn(ItemStack itemStack, Player player, InteractionHand hand, BlockHitResult hit) {
        // By default, this passes to useWithoutItem
        return ItemInteractionResult.PASS_TO_DEFAULT_BLOCK_INTERACTION;
    }

    default void onBlockClicked(Player player) { }

    default void onEntityCollision(Entity entity) { }
    default void onEntityWalk(Entity entity) { }

    default void onNeighborBlockChanged(BlockPos neighborPos) { }
    default void onNeighborTileChanged(BlockPos neighborPos) { }

    default boolean getWeakChanges() { return false; } //True if listening to weak changes

    default boolean canRedstoneConnect(int side) { return false; }
    default int getStrongPower(int side) { return 0; }
    default int getWeakPower(int side) { return 0; }

    void saveToNBT(CompoundTag tag, HolderLookup.Provider lookupProvider);
    void loadFromNBT(CompoundTag tag, HolderLookup.Provider lookupProvider);

    void writeDesc(MCDataOutput out);
    void readDesc(MCDataInput in);
}
