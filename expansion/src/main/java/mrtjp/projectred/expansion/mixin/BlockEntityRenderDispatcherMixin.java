package mrtjp.projectred.expansion.mixin;

import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.expansion.MovementManager;
import mrtjp.projectred.expansion.client.MovementClientRegistry;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderDispatcher;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.world.level.block.entity.BlockEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(BlockEntityRenderDispatcher.class)
public class BlockEntityRenderDispatcherMixin {

    @Redirect(
            method = "setupAndRender(Lnet/minecraft/client/renderer/blockentity/BlockEntityRenderer;Lnet/minecraft/world/level/block/entity/BlockEntity;FLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;)V",
            at = @At(
                    value = "INVOKE",
                    target = "net/minecraft/client/renderer/blockentity/BlockEntityRenderer.render (Lnet/minecraft/world/level/block/entity/BlockEntity;FLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;II)V"
            )
    )
    private static <E extends BlockEntity> void wrapSetupAndRender(BlockEntityRenderer<E> renderer, E blockEntity, float partialTicks, PoseStack pStack, MultiBufferSource buffers, int packedLight, int packedOverlay) {

        if (blockEntity.getLevel() == null) {
            // Call wrapped target and bail
            renderer.render(blockEntity, partialTicks, pStack, buffers, packedLight, packedOverlay);
            return;
        }

        // Get movement info
        MovementManager.InternalMovementInfo info = MovementManager.getInstance(blockEntity.getLevel()).getMovementInfo(blockEntity.getBlockPos());

        // Translate stack if moving
        if (info.isMoving()) {
            Vector3 offset = info.getRenderOffset(partialTicks);
            pStack.pushPose();
            pStack.translate(offset.x, offset.y, offset.z);
            MovementClientRegistry.dispatchPreRender(offset.x, offset.y, offset.z);
        }

        // Call wrapped target
        renderer.render(blockEntity, partialTicks, pStack, buffers, packedLight, packedOverlay);

        // Pop stack if moving
        if (info.isMoving()) {
            pStack.popPose();
            MovementClientRegistry.dispatchPostRender();
        }
    }

//    @Inject(
//            method = "setupAndRender(Lnet/minecraft/client/renderer/blockentity/BlockEntityRenderer;Lnet/minecraft/world/level/block/entity/BlockEntity;FLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;)V",
//            at = @At("HEAD"),
//            cancellable = true
//    )
//    private static <E extends BlockEntity> void preSetupAndRender(BlockEntityRenderer<E> renderer, E blockEntity, float partialTicks, PoseStack pStack, MultiBufferSource buffers, CallbackInfo ci) {
//    }
//
//    @Inject(
//            method = "setupAndRender(Lnet/minecraft/client/renderer/blockentity/BlockEntityRenderer;Lnet/minecraft/world/level/block/entity/BlockEntity;FLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;)V",
//            at = @At("RETURN")
//    )
//    private static <E extends BlockEntity> void postSetupAndRender(BlockEntityRenderer<E> renderer, E blockEntity, float partialTicks, PoseStack pStack, MultiBufferSource buffers, CallbackInfo ci) {
//    }
}
