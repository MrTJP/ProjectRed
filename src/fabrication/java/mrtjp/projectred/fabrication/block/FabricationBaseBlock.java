package mrtjp.projectred.fabrication.block;

import mrtjp.projectred.core.tile.IBlockEventTile;
import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.material.Material;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.item.ItemStack;
import net.minecraft.state.BooleanProperty;
import net.minecraft.state.DirectionProperty;
import net.minecraft.state.StateContainer;
import net.minecraft.state.properties.BlockStateProperties;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.IWorldReader;
import net.minecraft.world.World;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class FabricationBaseBlock extends Block {

    public static final DirectionProperty ROTATION_PROPERTY = BlockStateProperties.HORIZONTAL_FACING;
    public static final BooleanProperty HAS_BLUEPRINT_PROPERTY = BooleanProperty.create("blueprint");

    private final Supplier<TileEntity> tileEntitySupplier;

    public FabricationBaseBlock(Supplier<TileEntity> tileEntitySupplier) {
        super(AbstractBlock.Properties.of(Material.STONE));
        this.tileEntitySupplier = tileEntitySupplier;
    }

    @Override
    public boolean hasTileEntity(BlockState state) {
        return true;
    }

    @Nullable
    @Override
    public TileEntity createTileEntity(BlockState state, IBlockReader world) {
        TileEntity tile = tileEntitySupplier.get();
        if (tile instanceof IBlockEventTile) ((IBlockEventTile) tile).loadBlockState(state);
        return tile;
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockItemUseContext context) {
        return defaultBlockState()
                .setValue(ROTATION_PROPERTY, context.getHorizontalDirection().getOpposite())
                .setValue(HAS_BLUEPRINT_PROPERTY, false);
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

    @Override
    protected void createBlockStateDefinition(StateContainer.Builder<Block, BlockState> builder) {
        builder.add(ROTATION_PROPERTY);
        builder.add(HAS_BLUEPRINT_PROPERTY);
    }
}
