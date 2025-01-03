package mrtjp.projectred.core.block;

import mrtjp.projectred.core.tile.IBlockEventBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.EntityBlock;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityTicker;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.block.state.properties.IntegerProperty;
import net.minecraft.world.level.material.Material;
import net.minecraft.world.phys.BlockHitResult;
import org.jetbrains.annotations.Nullable;

public abstract class ProjectRedBlock extends Block implements EntityBlock {

    public static final IntegerProperty SIDE = IntegerProperty.create("side", 0, 5);
    public static final IntegerProperty ROTATION = IntegerProperty.create("rotation", 0, 3);
    public static final BooleanProperty CHARGED = BooleanProperty.create("charged");
    public static final BooleanProperty WORKING = BooleanProperty.create("working");
    public static final BooleanProperty ACTIVE = BooleanProperty.create("active");

    public static final BlockBehaviour.Properties WOODEN_PROPERTIES = BlockBehaviour.Properties.of(Material.WOOD).strength(2.0F, 3.0F).sound(SoundType.WOOD);
    public static final BlockBehaviour.Properties STONE_PROPERTIES = BlockBehaviour.Properties.of(Material.STONE).requiresCorrectToolForDrops().strength(3.0F, 3.0F).sound(SoundType.STONE);

    public ProjectRedBlock(BlockBehaviour.Properties properties) {
        super(properties);
    }

    protected abstract BlockEntityType<?> getBlockEntityType();

    @Nullable
    @Override
    public <T extends BlockEntity> BlockEntityTicker<T> getTicker(Level level, BlockState state, BlockEntityType<T> type) {
        if (type != getBlockEntityType()) return null;
        return (level1, pos, state1, tile) -> {
            if (tile instanceof IBlockEventBlockEntity t) {
                t.tick();
            }
        };
    }

    @Override
    public void neighborChanged(BlockState state, Level world, BlockPos pos, Block blockIn, BlockPos neighbor, boolean isMoving) {
        BlockEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventBlockEntity) ((IBlockEventBlockEntity) tile).onNeighborBlockChanged(neighbor);
    }

    @Override
    public void onNeighborChange(BlockState state, LevelReader world, BlockPos pos, BlockPos neighbor) {
        BlockEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventBlockEntity) ((IBlockEventBlockEntity) tile).onNeighborTileChanged(neighbor);
    }

    @Override
    public InteractionResult use(BlockState state, Level world, BlockPos pos, Player player, InteractionHand hand, BlockHitResult hit) {
        BlockEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventBlockEntity) return ((IBlockEventBlockEntity) tile).onBlockActivated(player, hand, hit);
        return InteractionResult.FAIL; //TODO pass?
    }

    @Override
    public void onRemove(BlockState oldState, Level world, BlockPos pos, BlockState newState, boolean isMoving) {
        BlockEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventBlockEntity) {
            if (oldState.is(newState.getBlock())) {
                ((IBlockEventBlockEntity) tile).onBlockStateReplaced(newState);
            } else {
                ((IBlockEventBlockEntity) tile).onBlockRemoved();
            }
        }
        super.onRemove(oldState, world, pos, newState, isMoving); // Removes tile if no longer valid for new state
    }

    @Override
    public void setPlacedBy(Level world, BlockPos pos, BlockState state, @Nullable LivingEntity player, ItemStack stack) {
        BlockEntity tile = world.getBlockEntity(pos);
        if (tile instanceof IBlockEventBlockEntity) ((IBlockEventBlockEntity) tile).onBlockPlaced(player, stack);
    }
}
