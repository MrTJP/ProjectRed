package mrtjp.projectred.expansion.block;

import codechicken.lib.render.particle.CustomParticleHandler;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.expansion.client.FrameMotorBlockRenderer;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import mrtjp.projectred.expansion.tile.FrameMotorTile;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.core.BlockPos;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraftforge.client.IBlockRenderProperties;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.function.Consumer;

public class FrameMotorBlock extends ProjectRedBlock {

    public FrameMotorBlock() {
        super(STONE_PROPERTIES);
    }

    @Override
    public void initializeClient(Consumer<IBlockRenderProperties> consumer) {
        consumer.accept(new IBlockRenderProperties() {
            @Override
            public boolean addHitEffects(BlockState state, Level level, HitResult hit, ParticleEngine engine) {
                if (!(hit instanceof BlockHitResult blockHit)) return false;
                CustomParticleHandler.addBlockHitEffects(
                        level,
                        Cuboid6.full.copy().add(blockHit.getBlockPos()),
                        blockHit.getDirection(),
                        FrameMotorBlockRenderer.getParticleIcon(state, blockHit.getDirection().ordinal()),
                        engine
                );
                return true;
            }

            @Override
            public boolean addDestroyEffects(BlockState state, Level level, BlockPos pos, ParticleEngine engine) {
                CustomParticleHandler.addBlockDestroyEffects(
                        level,
                        Cuboid6.full.copy().add(pos),
                        Collections.singletonList(FrameMotorBlockRenderer.getParticleIcon(state, 0)),
                        engine);
                return true;
            }
        });
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new FrameMotorTile(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return ExpansionReferences.FRAME_MOTOR_TILE;
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        int s = context.getClickedFace().ordinal() ^ 1; // Place bottom of block on the side clicked
        int r = context.getPlayer() == null ? 0 : (Rotation.getSidedRotation(context.getPlayer(), context.getClickedFace().ordinal()) + 2) % 4;
        return defaultBlockState()
                .setValue(SIDE, s)
                .setValue(ROTATION, r)
                .setValue(CHARGED, false)
                .setValue(WORKING, false);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        builder.add(SIDE, ROTATION, CHARGED, WORKING);
    }
}
