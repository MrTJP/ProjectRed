package mrtjp.projectred.core.block;

import mrtjp.projectred.core.tile.IBlockEventTile;
import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.state.BooleanProperty;
import net.minecraft.state.IntegerProperty;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.IWorldReader;
import net.minecraft.world.World;

import javax.annotation.Nullable;

public abstract class ProjectRedBlock extends Block {

    public static final IntegerProperty SIDE = IntegerProperty.create("side", 0, 5);
    public static final IntegerProperty ROTATION = IntegerProperty.create("rotation", 0, 3);
    public static final BooleanProperty CHARGED = BooleanProperty.create("charged");
    public static final BooleanProperty WORKING = BooleanProperty.create("working");

    public ProjectRedBlock(AbstractBlock.Properties properties) {
        super(properties);
    }

    protected abstract TileEntity createTileEntityInstance(BlockState state, IBlockReader world);

    @Override
    public boolean hasTileEntity(BlockState state) {
        return true;
    }

    @Nullable
    @Override
    public TileEntity createTileEntity(BlockState state, IBlockReader world) {
        TileEntity tile = createTileEntityInstance(state, world);
        if (tile instanceof IBlockEventTile) ((IBlockEventTile) tile).loadBlockState(state);
        return tile;
    }

    @Override
    public void neighborChanged(BlockState state, World world, BlockPos pos, Block blockIn, BlockPos neighbor, boolean isMoving) {
        TileEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventTile) ((IBlockEventTile) tile).onNeighborBlockChanged(neighbor);
    }

    @Override
    public void onNeighborChange(BlockState state, IWorldReader world, BlockPos pos, BlockPos neighbor) {
        TileEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventTile) ((IBlockEventTile) tile).onNeighborTileChanged(neighbor);
    }

    @Override
    public ActionResultType use(BlockState state, World world, BlockPos pos, PlayerEntity player, Hand hand, BlockRayTraceResult hit) {
        TileEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventTile) return ((IBlockEventTile) tile).onBlockActivated(player, hand, hit);
        return ActionResultType.FAIL; //TODO pass?
    }

    @Override
    public void onRemove(BlockState oldState, World world, BlockPos pos, BlockState newState, boolean isMoving) {
        TileEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventTile) {
            if (oldState.is(newState.getBlock())) {
                ((IBlockEventTile) tile).onBlockStateReplaced(newState);
            } else {
                ((IBlockEventTile) tile).onBlockRemoved();
            }
        }
        super.onRemove(oldState, world, pos, newState, isMoving); // Removes tile if no longer valid for new state
    }

    @Override
    public void setPlacedBy(World world, BlockPos pos, BlockState state, @Nullable LivingEntity player, ItemStack stack) {
        TileEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventTile) ((IBlockEventTile) tile).onBlockPlaced(player, stack);
    }
}
