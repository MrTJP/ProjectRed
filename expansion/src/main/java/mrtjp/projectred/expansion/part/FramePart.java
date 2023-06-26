package mrtjp.projectred.expansion.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.part.MicroblockPart;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.part.*;
import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.api.Frame;
import mrtjp.projectred.expansion.block.FrameBlock;
import mrtjp.projectred.expansion.client.FrameModelRenderer;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.phys.shapes.BooleanOp;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;

import static mrtjp.projectred.expansion.init.ExpansionReferences.FRAME_BLOCK;

public class FramePart extends BaseMultipart implements NormalOcclusionPart, IconHitEffectsPart, Frame {

    public static final Cuboid6[] oBounds = new Cuboid6[6];
    public static final VoxelShape[] oShapes = new VoxelShape[6];

    private static int sideOccludeTest = -1;

    static {
        for (int s = 0; s < 6; s++) {
            Transformation transform = Rotation.sideRotations[s].at(Vector3.CENTER);
            double d = 4/16D;
            Cuboid6 box = new Cuboid6(d, 0, d, 1-d, d, 1-d).apply(transform);

            oBounds[s] = box;
            oShapes[s] = VoxelShapeCache.getShape(box);
        }
    }

    private byte occludedSidesMask = 0;

    //region save/load
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putByte("omask", occludedSidesMask);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        occludedSidesMask = tag.getByte("omask");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(occludedSidesMask);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        occludedSidesMask = packet.readByte();
    }
    //endregion

    //region Frame
    @Override
    public boolean canGrab(Level w, BlockPos pos, Direction side) {
        return !isSideCovered(side.ordinal());
    }

    @Override
    public boolean canBeGrabbed(Level w, BlockPos pos, Direction side) {
        return !isSideCovered(side.ordinal());
    }

    private boolean isSideCovered(int side) {
        return tile().getSlottedPart(side) instanceof MicroblockPart mb && mb.getSize() == 1;
    }
    //endregion

    //region Events
    @Override
    public void onAdded() {
        if (!level().isClientSide) calcAndSendOccludedSidesMask();
    }

    @Override
    public void onNeighborBlockChanged(BlockPos neighbor) {
        if (!level().isClientSide) calcAndSendOccludedSidesMask();
    }

    @Override
    public void onPartChanged(@Nullable MultiPart part) {
        if (!level().isClientSide) calcAndSendOccludedSidesMask();
    }

    @Override
    public void onMoved() {
        super.onMoved();
        if (!level().isClientSide) calcAndSendOccludedSidesMask();
    }
    //endregion

    //region MultiPart properties
    @Override
    public MultipartType<?> getType() {
        return ExpansionReferences.FRAME_PART;
    }

    @Override
    public float getStrength(Player player, PartRayTraceResult hit) {
        return FRAME_BLOCK.defaultBlockState()
                .getDestroyProgress(player, player.level, new BlockPos(0, -1, 0));
    }

    @Override
    public @Nullable SoundType getPlacementSound(UseOnContext context) {
        return FRAME_BLOCK.defaultBlockState().getSoundType();
    }

    @Override
    public ItemStack getCloneStack(PartRayTraceResult hit) {
        return new ItemStack(FRAME_BLOCK);
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Collections.singleton(new ItemStack(FRAME_BLOCK));
    }
    //endregion

    //region Part shapes
    @Override
    public VoxelShape getShape(CollisionContext context) {
        return FrameBlock.getOrGenerateShape(getOccludedSideMask());
    }

    @Override
    public VoxelShape getCollisionShape(CollisionContext context) {
        return Shapes.block();
    }
    //endregion

    //region Occlusion
    @Override
    public VoxelShape getOcclusionShape() {
        return sideOccludeTest != -1 ? oShapes[sideOccludeTest] : Shapes.empty();
    }

    @Override
    public boolean occlusionTest(MultiPart nPart) {

        // Only one instance of FramePart allowed per block
        if (nPart instanceof FramePart) return false;

        // If not updating sideOccludeMask, just do normal occlusion test
        if (sideOccludeTest == -1) return NormalOcclusionPart.super.occlusionTest(nPart);

        // Similar to normal occlusion test but includes collision boxes of nPart as well
        VoxelShape shape = nPart.getCollisionShape(CollisionContext.empty());
        if (nPart instanceof NormalOcclusionPart p) {
            shape = Shapes.or(shape, p.getOcclusionShape());
        }
        if (nPart instanceof PartialOcclusionPart p) {
            shape = Shapes.or(shape, p.getPartialOcclusionShape());
        }

        return !Shapes.joinIsNotEmpty(shape, getOcclusionShape(), BooleanOp.AND);
    }

    public int getOccludedSideMask() {
        return occludedSidesMask & 0xFF;
    }

    private void calcAndSendOccludedSidesMask() {
        byte oldMask = occludedSidesMask;
        occludedSidesMask = 0;

        for (int s = 0; s < 6; s++) {
            sideOccludeTest = s;
            if (!tile().canReplacePart(this, this)) occludedSidesMask |= 1 << s;
        }
        sideOccludeTest = -1;

        if (oldMask != occludedSidesMask) {
            sendUpdate(this::writeDesc);
        }
    }
   //endregion

    //region Hit effects
    @Override
    public Cuboid6 getBounds() {
        return Cuboid6.full;
    }

    @Override
    public TextureAtlasSprite getBreakingIcon(PartRayTraceResult hit) {
        return FrameModelRenderer.getFrameIcon();
    }

    @Override
    public TextureAtlasSprite getBrokenIcon(int side) {
        return FrameModelRenderer.getFrameIcon();
    }
    //endregion
}
