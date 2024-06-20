package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.tile.ProjectRedTile;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;

public abstract class BaseDeviceTile extends ProjectRedTile {

    protected int side = 0;
    protected boolean powered = false;
    protected boolean active = false;

    private long schedTick = -1L;

    public BaseDeviceTile(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        side = state.getValue(ProjectRedBlock.SIDE);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        tag.putLong("schedTick", schedTick);
        tag.putByte("side", (byte) side);
        tag.putBoolean("powered", powered);
        tag.putBoolean("active", active);
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
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
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        ItemStack held = player.getItemInHand(hand);

        // Try to rotate block
        if (held.getItem() instanceof IScrewdriver screwdriver) {
            if (screwdriver.canUse(player, held)) {
                if (!level.isClientSide) {
                    side = (side + 1) % 6;
                    screwdriver.damageScrewdriver(player, held);
                    pushBlockState();
                    setChanged();
                }
                return InteractionResult.sidedSuccess(level.isClientSide);
            }
            return InteractionResult.FAIL;
        }
        return InteractionResult.PASS;
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
