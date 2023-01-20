package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.tile.ProjectRedTile;
import net.minecraft.block.BlockState;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.tileentity.TileEntityType;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;

public abstract class BaseDeviceTile extends ProjectRedTile implements ITickableTileEntity {

    protected int side = 0;
    protected boolean powered = false;
    protected boolean active = false;

    private long schedTick = -1L;

    public BaseDeviceTile(TileEntityType<?> type) {
        super(type);
    }

    @Override
    public void saveToNBT(CompoundNBT tag) {
        tag.putLong("schedTick", schedTick);
        tag.putByte("side", (byte) side);
        tag.putBoolean("powered", powered);
        tag.putBoolean("active", active);
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
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

        if (!powered) {
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
    public ActionResultType onBlockActivated(PlayerEntity player, Hand hand, BlockRayTraceResult hit) {
        ItemStack held = player.getItemInHand(hand);

        // Try to rotate block
        if (held.getItem() instanceof IScrewdriver) {
            IScrewdriver screwdriver = (IScrewdriver) held.getItem();
            if (screwdriver.canUse(player, held)) {
                if (!level.isClientSide) {
                    side = (side + 1) % 6;
                    screwdriver.damageScrewdriver(player, held);
                    pushBlockState();
                    setChanged();
                }
                return ActionResultType.sidedSuccess(level.isClientSide);
            }
            return ActionResultType.FAIL;
        }
        return ActionResultType.PASS;
    }

    @Override
    public void loadBlockState(BlockState state) {
        super.loadBlockState(state);
        side = state.getValue(ProjectRedBlock.SIDE);
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
    //endregion
}
