package mrtjp.projectred.core.tile;

import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.block.ProjectRedBlock;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;

public interface IOrientableBlockEntity extends IBlockEventBlockEntity {

    default int getRotation() {
        return getBlockLevel().getBlockState(getBlockPosition()).getValue(ProjectRedBlock.ROTATION);
    }

    default void setRotation(int r) {
        BlockState state = getBlockLevel().getBlockState(getBlockPosition()).setValue(ProjectRedBlock.ROTATION, r);
        getBlockLevel().setBlockAndUpdate(getBlockPosition(), state);
    }

    default int getSide() {
        return getBlockLevel().getBlockState(getBlockPosition()).getValue(ProjectRedBlock.SIDE);
    }

    default void setSide(int s) {
        BlockState state = getBlockLevel().getBlockState(getBlockPosition()).setValue(ProjectRedBlock.SIDE, s);
        getBlockLevel().setBlockAndUpdate(getBlockPosition(), state);
    }

    @Override
    default InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        ItemStack held = player.getItemInHand(hand);

        // Try to rotate block
        if (held.getItem() instanceof IScrewdriver screwdriver) {
            if (screwdriver.canUse(player, held)) {
                if (!getBlockLevel().isClientSide) {
                    if ((player.isShiftKeyDown() || !canOrient()) && canRotate()) {
                        rotateBlock();
                    } else if (canOrient()) {
                        orientBlock();
                    }
                    screwdriver.damageScrewdriver(player, held);
                }
                return InteractionResult.sidedSuccess(getBlockLevel().isClientSide);
            }
            return InteractionResult.FAIL;
        }

        return InteractionResult.PASS;
    }

    default boolean canRotate() {
        return getBlockLevel().getBlockState(getBlockPosition()).hasProperty(ProjectRedBlock.ROTATION);
    }

    default boolean canOrient() {
        return getBlockLevel().getBlockState(getBlockPosition()).hasProperty(ProjectRedBlock.SIDE);
    }

    default void rotateBlock() {
        setRotation((getRotation() + 1) % 4);
        onOrientationChange();
    }

    default void orientBlock() {
        setSide((getSide() + 1) % 6);
        onOrientationChange();
    }

    void onOrientationChange();
}
