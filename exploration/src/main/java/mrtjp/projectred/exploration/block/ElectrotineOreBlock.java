package mrtjp.projectred.exploration.block;

import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.BlockItem;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.item.ItemStack;
import net.minecraft.particles.RedstoneParticleData;
import net.minecraft.state.BooleanProperty;
import net.minecraft.state.StateContainer;
import net.minecraft.state.properties.BlockStateProperties;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.ToolType;

import java.util.Random;

/**
 * All methods lifted straight from RedstoneOreBlock
 */
public class ElectrotineOreBlock extends OreBlock {

    public static final BooleanProperty LIT = BlockStateProperties.LIT;

    public static final RedstoneParticleData ELECTROTINE_PARTICLE = new RedstoneParticleData(
            15 / 255F, 103 / 255F, 178 / 255F, 0.6F);

    public ElectrotineOreBlock(int harvestLevel, int minExp, int maxExp) {
        super(AbstractBlock.Properties.of(Material.STONE)
                .strength(3.0F, 3.0F)
                .harvestLevel(harvestLevel)
                .requiresCorrectToolForDrops()
                .harvestTool(ToolType.PICKAXE)
                .sound(SoundType.STONE)
                .lightLevel(s -> s.getValue(LIT) ? 9 : 0), harvestLevel, minExp, maxExp);

        registerDefaultState(defaultBlockState().setValue(LIT, false));
    }

    public void attack(BlockState state, World world, BlockPos pos, PlayerEntity player) {
        interact(state, world, pos);
        super.attack(state, world, pos, player);
    }

    public void stepOn(World world, BlockPos pos, Entity player) {
        interact(world.getBlockState(pos), world, pos);
        super.stepOn(world, pos, player);
    }

    public ActionResultType use(BlockState state, World world, BlockPos pos, PlayerEntity player, Hand hand, BlockRayTraceResult rayTraceResult) {
        if (world.isClientSide) {
            spawnParticles(world, pos);
        } else {
            interact(state, world, pos);
        }

        ItemStack itemstack = player.getItemInHand(hand);
        return itemstack.getItem() instanceof BlockItem && (new BlockItemUseContext(player, hand, itemstack, rayTraceResult)).canPlace() ? ActionResultType.PASS : ActionResultType.SUCCESS;
    }

    private static void interact(BlockState state, World world, BlockPos pos) {
        spawnParticles(world, pos);
        if (!state.getValue(LIT)) {
            world.setBlock(pos, state.setValue(LIT, true), 3);
        }
    }

    public boolean isRandomlyTicking(BlockState state) {
        return state.getValue(LIT);
    }

    public void randomTick(BlockState state, ServerWorld world, BlockPos pos, Random random) {
        if (state.getValue(LIT)) {
            world.setBlock(pos, state.setValue(LIT, false), 3);
        }
    }

    @Override
    public void animateTick(BlockState state, World world, BlockPos pos, Random random) {
        if (state.getValue(LIT)) {
            spawnParticles(world, pos);
        }
    }

    private static void spawnParticles(World world, BlockPos pos) {
        double d0 = 0.5625D;
        Random random = world.random;

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

    protected void createBlockStateDefinition(StateContainer.Builder<Block, BlockState> stateBuilder) {
        stateBuilder.add(LIT);
    }
}
