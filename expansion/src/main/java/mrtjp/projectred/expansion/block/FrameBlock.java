package mrtjp.projectred.expansion.block;

import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.*;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.api.Frame;
import mrtjp.projectred.expansion.client.FrameModelRenderer;
import mrtjp.projectred.expansion.client.FrameModelVerts;
import mrtjp.projectred.lib.ModelVoxelShape;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Material;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.fml.DistExecutor;

import java.util.LinkedList;
import java.util.List;

public class FrameBlock extends Block implements Frame {

    private static final VoxelShape[] shapes = new VoxelShape[64];

    public FrameBlock() {
        super(BlockBehaviour.Properties.of(Material.WOOD)
                .strength(2.0F)
                .sound(SoundType.WOOD)
                .dynamicShape()); // To prevent early caching before modelVerts can be loaded
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
        return FrameBlock.getOrGenerateShape(0);
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

    //region Shape generation
    public static VoxelShape getOrGenerateShape(int mask) {
        VoxelShape s = shapes[mask & 0x3F];
        if (s == null) {
            s = generateShape(mask);
            shapes[mask & 0x3F] = s;
        }
        return s;
    }

    private static VoxelShape generateShape(int mask) {

        // bottom face cuboids
        double th = 2/16D; // Frame thickness
        Cuboid6 e0 = new Cuboid6(0, 0, 0, 1, th, th);
        Cuboid6 e1 = e0.copy().apply(Rotation.quarterRotations[1].at(Vector3.CENTER));
        Cuboid6 e2 = e0.copy().apply(Rotation.quarterRotations[2].at(Vector3.CENTER));
        Cuboid6 e3 = e0.copy().apply(Rotation.quarterRotations[3].at(Vector3.CENTER));

        List<VoxelShape> faceShapes = new LinkedList<>();
        for (int s = 0; s < 6; s++) {
            Transformation t = Rotation.sideOrientation(s, 0).at(Vector3.CENTER);
            ImmutableSet.Builder<VoxelShape> fb = ImmutableSet.builder();
            fb.add(VoxelShapeCache.getShape(e0.copy().apply(t)));
            fb.add(VoxelShapeCache.getShape(e2.copy().apply(t)));
            fb.add(VoxelShapeCache.getShape(e1.copy().apply(t)));
            fb.add(VoxelShapeCache.getShape(e3.copy().apply(t)));
            faceShapes.add(VoxelShapeCache.merge(fb.build()));
        }

        // Clients will use verts directly from parsed OBJ.
        // Server will use data-pack JSON file with verts from OBJ
        Vertex5[] verts = DistExecutor.unsafeRunForDist(
                () -> () -> FrameModelRenderer.getQuadsForMask(mask),
                () -> () -> getQuadsForMask(mask));

        VoxelShape parent = VoxelShapeCache.merge(ImmutableSet.copyOf(faceShapes));
        return ModelVoxelShape.fromQuads(parent, verts);
    }

    private static Vertex5[] getQuadsForMask(int mask) {
        List<Vertex5> verts = new LinkedList<>();
        verts.addAll(List.of(FrameModelVerts.verts.get("frame")));

        for (int i = 0; i < 6; i++) {
            if ((mask & (1 << i)) == 0) {
                Vertex5[] crossModel = FrameModelVerts.verts.get("cross_" + i);
                verts.addAll(List.of(crossModel));
            }
        }

        return verts.toArray(new Vertex5[0]);
    }
    //endregion
}
