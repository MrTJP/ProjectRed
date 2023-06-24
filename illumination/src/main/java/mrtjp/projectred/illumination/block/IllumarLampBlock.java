package mrtjp.projectred.illumination.block;

import codechicken.multipart.api.RedstoneConnectorBlock;
import mrtjp.projectred.illumination.tile.IllumarLampTile;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.EntityBlock;
import net.minecraft.world.level.block.RedstoneTorchBlock;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.material.Material;

import javax.annotation.Nullable;
import java.util.Random;

public class IllumarLampBlock extends Block implements EntityBlock, RedstoneConnectorBlock {

    public static final BooleanProperty LIT = RedstoneTorchBlock.LIT;

    private final int color;
    private final boolean inverted;

    public IllumarLampBlock(int color, boolean inverted) {
        super(BlockBehaviour.Properties.of(Material.BUILDABLE_GLASS)
                .strength(0.5F)
                .lightLevel((state) -> state.getValue(LIT) ? 15 : 0));

        this.color = color;
        this.inverted = inverted;
        registerDefaultState(defaultBlockState().setValue(LIT, inverted));
    }

    public int getColor() {
        return color;
    }

    public boolean isInverted() {
        return inverted;
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return defaultBlockState().setValue(LIT, context.getLevel().hasNeighborSignal(context.getClickedPos()) != inverted);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(LIT);
    }

    @Override
    public void neighborChanged(BlockState state, Level world, BlockPos pos, Block blockIn, BlockPos neighbor, boolean isMoving) {
        super.onNeighborChange(state, world, pos, neighbor);
        if (world.isClientSide()) return;

        boolean isLit = state.getValue(LIT);
        boolean shouldBeLit = world.hasNeighborSignal(pos) != inverted;

        if (isLit != shouldBeLit) {
            if (!world.getBlockTicks().hasScheduledTick(pos, this)) {
                world.scheduleTick(pos, this, 2);
            }
        }
    }

    @Override
    public void tick(BlockState state, ServerLevel world, BlockPos pos, Random rand) {
        super.tick(state, world, pos, rand);

        boolean isLit = state.getValue(LIT);
        boolean shouldBeLit = world.hasNeighborSignal(pos) != inverted;

        if (isLit != shouldBeLit) {
            world.setBlockAndUpdate(pos, state.setValue(LIT, shouldBeLit));
        }
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new IllumarLampTile(color, inverted, pos, state);
    }

    //region Redstone Connector block
    @Override
    public int getConnectionMask(LevelReader world, BlockPos pos, int side) {
        return 0x1F;
    }

    @Override
    public int weakPowerLevel(LevelReader world, BlockPos pos, int side, int mask) {
        return 0;
    }
    //endregion
}
