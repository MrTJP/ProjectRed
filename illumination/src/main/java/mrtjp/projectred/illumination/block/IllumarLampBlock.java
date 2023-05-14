package mrtjp.projectred.illumination.block;

import mrtjp.projectred.illumination.tile.IllumarLampTile;
import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.RedstoneTorchBlock;
import net.minecraft.block.material.Material;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.state.BooleanProperty;
import net.minecraft.state.StateContainer;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;

import javax.annotation.Nullable;
import java.util.Random;

public class IllumarLampBlock extends Block {

    public static final BooleanProperty LIT = RedstoneTorchBlock.LIT;

    private final int color;
    private final boolean inverted;

    public IllumarLampBlock(int color, boolean inverted) {
        super(AbstractBlock.Properties.of(Material.BUILDABLE_GLASS)
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
    public BlockState getStateForPlacement(BlockItemUseContext context) {
        return defaultBlockState().setValue(LIT, context.getLevel().hasNeighborSignal(context.getClickedPos()) != inverted);
    }

    @Override
    protected void createBlockStateDefinition(StateContainer.Builder<Block, BlockState> builder) {
        builder.add(LIT);
    }

    @Override
    public void neighborChanged(BlockState state, World world, BlockPos pos, Block blockIn, BlockPos neighbor, boolean isMoving) {
        super.onNeighborChange(state, world, pos, neighbor);
        if (world.isClientSide()) return;

        boolean isLit = state.getValue(LIT);
        boolean shouldBeLit = world.hasNeighborSignal(pos) != inverted;

        if (isLit != shouldBeLit) {
            if (!world.getBlockTicks().hasScheduledTick(pos, this)) {
                world.getBlockTicks().scheduleTick(pos, this, 2);
            }
        }
    }

    @Override
    public void tick(BlockState state, ServerWorld world, BlockPos pos, Random rand) {
        super.tick(state, world, pos, rand);

        boolean isLit = state.getValue(LIT);
        boolean shouldBeLit = world.hasNeighborSignal(pos) != inverted;

        if (isLit != shouldBeLit) {
            world.setBlockAndUpdate(pos, state.setValue(LIT, shouldBeLit));
        }
    }

    @Override
    public boolean hasTileEntity(BlockState state) {
        return true;
    }

    @Nullable
    @Override
    public TileEntity createTileEntity(BlockState state, IBlockReader world) {
        return new IllumarLampTile(color, inverted);
    }
}
