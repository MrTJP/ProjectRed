package mrtjp.projectred.expansion.tile;

import codechicken.lib.util.ItemUtils;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.api.tile.RedstoneConnector;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.core.tile.ProjectRedBlockEntity;
import mrtjp.projectred.expansion.part.PneumaticTubePayload;
import mrtjp.projectred.expansion.pneumatics.PneumaticQueue;
import mrtjp.projectred.expansion.pneumatics.PneumaticTransportContainer;
import mrtjp.projectred.expansion.pneumatics.PneumaticTransportDevice;
import mrtjp.projectred.expansion.pneumatics.PneumaticTransportMode;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Objects;

public abstract class BasePneumaticDeviceBlockEntity extends BaseDeviceBlockEntity implements PneumaticTransportDevice, RedstoneConnector {

    protected final PneumaticQueue itemQueue = new PneumaticQueue();

    public BasePneumaticDeviceBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    //region save/load
    @Override
    public void saveToNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.saveToNBT(tag, lookupProvider);
        CompoundTag queueTag = new CompoundTag();
        itemQueue.save(queueTag, lookupProvider);
        tag.put("queue", queueTag);
    }

    @Override
    public void loadFromNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.loadFromNBT(tag, lookupProvider);
        itemQueue.load(tag.getCompound("queue"), lookupProvider);
    }
    //endregion

    //region BlockEntity events
    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        while (!itemQueue.isEmpty()) {
            ProjectRedBlockEntity.dropItem(
                    Objects.requireNonNull(itemQueue.poll()).getItemStack(),
                    getLevel(),
                    Vector3.fromTileCenter(this));
        }
    }

    @Override
    protected void onScheduledTick() {
        super.onScheduledTick();

        if (!getLevel().isClientSide) {
            exportQueue();
            scheduleTick(itemQueue.isEmpty() ? 4 : 16);
        }
    }

    protected void exportQueue() {
        while (!itemQueue.isEmpty()) {
            var payload = itemQueue.poll();
            if (!exportToTube(payload) && !exportToItemEntity(payload)) {
                itemQueue.addBackStuffed(payload);
            }

            // For backstuffed items, we will export only 1 per call to exportQueue.
            if (itemQueue.isBackStuffed()) {
                break;
            }
        }
    }

    protected boolean exportToTube(PneumaticTubePayload payload) {
        var lookup = CenterLookup.lookupStraightCenter(getLevel(), getBlockPos(), side);
        if (!(lookup.part instanceof PneumaticTransportContainer ptc)) return false;

        return ptc.insertPayload(lookup.otherDirection, payload);
    }

    protected boolean exportToItemEntity(PneumaticTubePayload payload) {
        CenterLookup lookup = CenterLookup.lookupStraightCenter(getLevel(), getBlockPos(), side);
        if (!lookup.state.isAir()) return false;

//        ProjectRedTile.ejectItem(payload.getItemStack(), getLevel(), getBlockPos(), side, 0.25);
        ItemUtils.ejectItem(getLevel(), getBlockPos().relative(Direction.values()[side]), payload.getItemStack(), Direction.values()[side]);
        return true;
    }
    //endregion

    //region PneumaticTransportDevice implementation
    @Override
    public boolean canConnectTube(int s) {
        return s == side || s == (side ^ 1);
    }

    @Override
    public boolean canAcceptPayload(int s, PneumaticTubePayload payload, PneumaticTransportMode mode) {
        return switch (mode) {
            case PASSIVE_NORMAL -> s == (side ^ 1) && !powered && itemQueue.isEmpty();
            case PASSIVE_BACKSTUFF -> s == side;
        };
    }

    @Override
    public boolean insertPayload(int s, PneumaticTubePayload payload) {
        if (canAcceptPayload(s, payload, PneumaticTransportMode.PASSIVE_NORMAL)) {
            itemQueue.add(payload);
            active = true; // Activate the device
            setChanged();
            pushBlockState();
            scheduleTick(4);
            exportQueue();
            return true;
        }

        if (canAcceptPayload(s, payload, PneumaticTransportMode.PASSIVE_BACKSTUFF)) {
            itemQueue.addBackStuffed(payload);
            active = true; // Activate the device
            setChanged();
            pushBlockState();
            scheduleTick(4);
            return true;
        }

        return false;
    }
    //endregion

    //region BaseDeviceTile implementations
    @Override
    protected boolean shouldStayActive() {
        // While items are in queue, remain active
        return !itemQueue.isEmpty();
    }
    //endregion

    //region Redstone connection
    @Override
    public int getConnectionMask(int side) {
        return (((side ^ 1) == this.side) ? 0 : 0x1F);
    }

    @Override
    public int weakPowerLevel(int side, int mask) {
        return 0;
    }
    //endregion
}
