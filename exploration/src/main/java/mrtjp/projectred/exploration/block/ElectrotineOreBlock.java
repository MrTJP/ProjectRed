package mrtjp.projectred.exploration.block;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.particles.DustParticleOptions;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.RandomSource;
import net.minecraft.util.valueproviders.IntProvider;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DropExperienceBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.phys.BlockHitResult;
import org.joml.Vector3f;

import java.util.Random;

/**
 * All methods lifted straight from RedstoneOreBlock
 */
public class ElectrotineOreBlock extends DropExperienceBlock {

    public static final BooleanProperty LIT = BlockStateProperties.LIT;

    public static final DustParticleOptions ELECTROTINE_PARTICLE = new DustParticleOptions(
            new Vector3f(15 / 255F, 103 / 255F, 178 / 255F), 0.6F);

    public ElectrotineOreBlock(BlockBehaviour.Properties properties, IntProvider xpRange) {
        super(properties.lightLevel(s -> s.getValue(LIT) ? 9 : 0), xpRange);

        registerDefaultState(defaultBlockState().setValue(LIT, false));
    }

    @Override
    public void attack(BlockState state, Level world, BlockPos pos, Player player) {
        interact(state, world, pos);
        super.attack(state, world, pos, player);
    }

    @Override
    public void stepOn(Level world, BlockPos pos, BlockState state, Entity player) {
        interact(world.getBlockState(pos), world, pos);
        super.stepOn(world, pos, state, player);
    }

    public InteractionResult use(BlockState state, Level world, BlockPos pos, Player player, InteractionHand hand, BlockHitResult rayTraceResult) {
        if (world.isClientSide) {
            spawnParticles(world, pos);
        } else {
            interact(state, world, pos);
        }

        ItemStack itemstack = player.getItemInHand(hand);
        return itemstack.getItem() instanceof BlockItem && (new BlockPlaceContext(player, hand, itemstack, rayTraceResult)).canPlace() ? InteractionResult.PASS : InteractionResult.SUCCESS;
    }

    private static void interact(BlockState state, Level world, BlockPos pos) {
        spawnParticles(world, pos);
        if (!state.getValue(LIT)) {
            world.setBlock(pos, state.setValue(LIT, true), 3);
        }
    }

    public boolean isRandomlyTicking(BlockState state) {
        return state.getValue(LIT);
    }

    public void randomTick(BlockState state, ServerLevel world, BlockPos pos, Random random) {
        if (state.getValue(LIT)) {
            world.setBlock(pos, state.setValue(LIT, false), 3);
        }
    }

    @Override
    public void animateTick(BlockState state, Level world, BlockPos pos, RandomSource random) {
        if (state.getValue(LIT)) {
            spawnParticles(world, pos);
        }
    }

    private static void spawnParticles(Level world, BlockPos pos) {
        double d0 = 0.5625D;
        RandomSource random = world.random;

        for (Direction direction : Direction.values()) {
            BlockPos blockpos = pos.relative(direction);
            if (!world.getBlockState(blockpos).isSolidRender(world, blockpos)) {
                Direction.Axis axis = direction.getAxis();
                double d1 = axis == Direction.Axis.X ? 0.5D + d0 * (double) direction.getStepX() : (double) random.nextFloat();
                double d2 = axis == Direction.Axis.Y ? 0.5D + d0 * (double) direction.getStepY() : (double) random.nextFloat();
                double d3 = axis == Direction.Axis.Z ? 0.5D + d0 * (double) direction.getStepZ() : (double) random.nextFloat();
                world.addParticle(ELECTROTINE_PARTICLE, (double) pos.getX() + d1, (double) pos.getY() + d2, (double) pos.getZ() + d3, 0.0D, 0.0D, 0.0D);
            }
        }
    }

    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> stateBuilder) {
        stateBuilder.add(LIT);
    }
}
