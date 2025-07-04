package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.tile.ProjectRedBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.ItemInteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;

public abstract class BaseDeviceBlockEntity extends ProjectRedBlockEntity {

    protected int side = 0;
    protected boolean powered = false;
    protected boolean active = false;

    private long schedTick = -1L;

    public BaseDeviceBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        side = state.getValue(ProjectRedBlock.SIDE);
    }

    @Override
    public void saveToNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        tag.putLong("schedTick", schedTick);
        tag.putByte("side", (byte) side);
        tag.putBoolean("powered", powered);
        tag.putBoolean("active", active);
    }

    @Override
    public void loadFromNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        schedTick = tag.getLong("schedTick");
        side = tag.getByte("side");
        powered = tag.getBoolean("powered");
        active = tag.getBoolean("active");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
    }

    @Override
    public void readDesc(MCDataInput in) {
    }

    @Override
    public void tick() {
        if (getLevel().isClientSide) return;

        // Run scheduled tick
        if (schedTick >= 0L && getLevel().getGameTime() >= schedTick) {
            schedTick = -1L;
            onScheduledTick();
            setChanged();
        }
    }

    //region Tick Scheduling
    protected void onScheduledTick() {

        if (getLevel().isClientSide) return;

        if (!powered && !shouldStayActive()) {
            active = false;
            pushBlockState();
            onDeactivated();
            setChanged();
        }
    }

    protected boolean isTickScheduled() {
        return schedTick >= 0;
    }

    protected void scheduleTick(int ticks) {
        long targetTime = getLevel().getGameTime() + ticks;
        if (schedTick > 0 && schedTick < targetTime) return;
        schedTick = targetTime;
        setChanged();
    }
    //endregion

    @Override
    public void onNeighborBlockChanged(BlockPos neighborPos) {
        super.onNeighborBlockChanged(neighborPos);

        if (getLevel().isClientSide) return;

        if (getLevel().hasNeighborSignal(getBlockPos())) {
            if (!powered) {
                powered = true;
                if (!active) {
                    active = true;
                    pushBlockState();
                    onActivated();
                }
                setChanged();
            }

        } else {
            if (active && !isTickScheduled()) {
                scheduleTick(4);
            }
            powered = false;
            setChanged();
        }
    }

    @Override
    public ItemInteractionResult useItemOn(ItemStack held, Player player, InteractionHand hand, BlockHitResult hit) {
        // Try to rotate block
        if (held.getItem() instanceof IScrewdriver screwdriver) {
            if (screwdriver.canUse(player, hand)) {
                if (!level.isClientSide) {
                    side = (side + 1) % 6;
                    screwdriver.damageScrewdriver(player, hand);
                    pushBlockState();
                    setChanged();
                }
                return ItemInteractionResult.sidedSuccess(level.isClientSide);
            }
            return ItemInteractionResult.FAIL;
        }
        return ItemInteractionResult.PASS_TO_DEFAULT_BLOCK_INTERACTION;
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(ProjectRedBlock.SIDE, side)
                .setValue(ProjectRedBlock.ACTIVE, active);
    }

    //region Device abstracts
    protected abstract void onActivated();

    protected abstract void onDeactivated();

    protected boolean shouldStayActive() {
        return false;
    }
    //endregion
}
