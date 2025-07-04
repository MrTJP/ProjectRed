package mrtjp.projectred.expansion.tile;

import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.tile.RedstoneConnector;
import mrtjp.projectred.api.Frame;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.api.MovementDescriptor;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.tile.IOrientableBlockEntity;
import mrtjp.projectred.expansion.MovementManager;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import javax.annotation.Nullable;
import java.util.Set;

public abstract class BaseFrameMoverBlockEntity extends LowLoadPoweredBlockEntity implements RedstoneConnector, IOrientableBlockEntity, Frame {

    protected boolean powered = false;
    protected boolean isCharged = false;
    protected boolean isWorking = false;

    private @Nullable MovementDescriptor descriptor = null;

    public BaseFrameMoverBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    //region Save/load
    @Override
    public void saveToNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.saveToNBT(tag, lookupProvider);
        tag.putBoolean("powered", powered);
    }

    @Override
    public void loadFromNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.loadFromNBT(tag, lookupProvider);
        powered = tag.getBoolean("powered");
    }
    //endregion

    protected boolean isMoving() {
        return descriptor != null && descriptor.isMoving() && descriptor.getProgress() < 1.0;
    }

    protected void drawPower(int structureSize) {
        // TODO Once conductor cant work, the movement becomes free
        if (canConductorWork())
            conductor.applyPower(-(100 + structureSize * 10)); // 100W plus 10W per moving block
    }

    protected void beginMove() {
        assert level != null;
        BlockPos blockInFront = getBlockPos().relative(Direction.values()[getFrontSide()]);
        if (level.isEmptyBlock(blockInFront)) return;
        // If target block already moving, or if we are moving, we cannot start a new move
        if (MovementManager.getInstance(level).getMovementInfo(blockInFront).isMoving()) return;
        if (MovementManager.getInstance(level).getMovementInfo(getBlockPos()).isMoving()) return;

        assert ProjectRedAPI.expansionAPI != null;
        Set<BlockPos> blocks = ProjectRedAPI.expansionAPI.getStructure(level, blockInFront, getBlockPos());
        MovementDescriptor desc = ProjectRedAPI.expansionAPI.beginMove(level, getMoveDir(), 1 / 16D, blocks);
        if (desc.getStatus() == MovementDescriptor.MovementStatus.MOVING) {
            descriptor = desc;
        }
    }

    @Override
    public void tick() {
        super.tick();

        assert level != null;
        if (level.isClientSide) return;

        isCharged = canConductorWork();
        isWorking = isMoving();

        if (isWorking) {
            assert descriptor != null;
            drawPower(descriptor.getSize());
        }

        // Update block state if needed
        BlockState state = getBlockState();
        if (isCharged != state.getValue(ProjectRedBlock.CHARGED) ||
                isWorking != state.getValue(ProjectRedBlock.WORKING)) {
            pushBlockState();
        }
    }

    @Override
    public void onNeighborBlockChanged(BlockPos neighborPos) {
        super.onNeighborBlockChanged(neighborPos);

        assert level != null;
        if (level.isClientSide) return;

        boolean oldPowered = powered;
        for (int s = 0; s < 6; s++) {
            powered = RedstoneInteractions.getPowerTo(level, getBlockPos(), s, 0x1F) > 0;
            if (powered) break;
        }

        if (!oldPowered && powered && !isWorking && canConductorWork()) {
            beginMove();
        }
    }

    @Override
    public void onOrientationChange() {
        if (!getLevel().isClientSide) {
            updateExternals();
            notifyExternals(0xF);
        }
    }

    @Override
    public boolean canConnectPart(IConnectable part, int s, int edgeRot) {
        // Dont allow power wires on front side. Redstone is handled by getConnectionMask
        return s != getFrontSide() && super.canConnectPart(part, s, edgeRot);
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(ProjectRedBlock.CHARGED, isCharged)
                .setValue(ProjectRedBlock.WORKING, isWorking);
    }

    @Override
    public int getConnectionMask(int side) {
        // Redstone may not connect on front side
        return side == getFrontSide() ? 0 : 0x1F;
    }

    @Override
    public int weakPowerLevel(int side, int mask) {
        return 0;
    }

    @Override
    public boolean canGrab(Level w, BlockPos pos, Direction side) {
        // If this block is moved, it will bring along whatever structure is touching its front face
        return side.ordinal() == getFrontSide();
    }

    @Override
    public boolean canBeGrabbed(Level w, BlockPos pos, Direction side) {
        // If a structure touching front face is moved by something else, it may not bring this block along
        return side.ordinal() != getFrontSide();
    }

    //region Mover abstracts
    /**
     * Direction towards the front of the mover. This BlockEntity's position offset to this frontSide
     * would contain the initial block of the structure to be moved.
     */
    protected abstract int getFrontSide();

    /**
     * Direction that the entire moving structure once resolved would move.
     */
    protected abstract int getMoveDir();
    //endregion
}
