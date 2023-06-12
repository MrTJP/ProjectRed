package mrtjp.projectred.expansion.block;

import codechicken.lib.render.particle.CustomParticleHandler;
import codechicken.lib.vec.Cuboid6;
import mrtjp.projectred.api.Frame;
import mrtjp.projectred.expansion.client.FrameModelRenderer;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Material;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.client.IBlockRenderProperties;

import java.util.Collections;
import java.util.function.Consumer;

public class FrameBlock extends Block implements Frame {

    public FrameBlock() {
        super(BlockBehaviour.Properties.of(Material.WOOD)
                .strength(2.0F)
                .sound(SoundType.WOOD));
    }

    @Override
    public void initializeClient(Consumer<IBlockRenderProperties> consumer) {
        consumer.accept(new IBlockRenderProperties() {
            @Override
            public boolean addHitEffects(BlockState state, Level level, HitResult hit, ParticleEngine engine) {
                if (!(hit instanceof BlockHitResult blockHit)) {
                    return false;
                }
                CustomParticleHandler.addBlockHitEffects(
                        level,
                        Cuboid6.full.copy().add(blockHit.getBlockPos()),
                        blockHit.getDirection(),
                        FrameModelRenderer.getFrameIcon(),
                        engine
                );
                return true;
            }

            @Override
            public boolean addDestroyEffects(BlockState state, Level level, BlockPos pos, ParticleEngine engine) {
                CustomParticleHandler.addBlockDestroyEffects(
                        level,
                        Cuboid6.full.copy().add(pos),
                        Collections.singletonList(FrameModelRenderer.getFrameIcon()),
                        engine);
                return true;
            }
        });
    }

    //region Frame
    @Override
    public boolean canGrab(Level w, BlockPos pos, Direction side) {
        return true;
    }

    @Override
    public boolean canBeGrabbed(Level w, BlockPos pos, Direction side) {
        return true;
    }
    //endregion

    //region Shapes
    @Override
    public VoxelShape getShape(BlockState state, BlockGetter level, BlockPos pos, CollisionContext context) {
        return FrameModelRenderer.getShape(0);
    }

    @Override
    public VoxelShape getVisualShape(BlockState state, BlockGetter level, BlockPos pos, CollisionContext context) {
        return getShape(state, level, pos, context);
    }

    @Override
    public VoxelShape getBlockSupportShape(BlockState state, BlockGetter level, BlockPos pos) {
        return getShape(state, level, pos, CollisionContext.empty());
    }

    @Override
    public VoxelShape getCollisionShape(BlockState state, BlockGetter level, BlockPos pos, CollisionContext context) {
        // Entities will collide as if this is a normal full block
        return Shapes.block();
    }
    //endregion
}
