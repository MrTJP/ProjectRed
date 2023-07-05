package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.expansion.ProjectRedExpansion;
import mrtjp.projectred.expansion.block.BatteryBoxBlock;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import mrtjp.projectred.expansion.inventory.container.BatteryBoxContainer;
import mrtjp.projectred.expansion.item.IChargable;
import mrtjp.projectred.expansion.item.IRechargableBattery;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Tuple;
import net.minecraft.world.*;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.wrapper.SidedInvWrapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static mrtjp.projectred.expansion.init.ExpansionReferences.BATTERY_BOX_BLOCK;

public class BatteryBoxTile extends LowLoadPoweredTile {

    public static final String TAG_KEY_POWER_STORED = "power_stored";
    public static final String TAG_KEY_CHARGE_LEVEL_STATE = "charge_level";

    private final BatteryBoxInventory inventory = new BatteryBoxInventory();
    private final LazyOptional<? extends IItemHandler>[] handlers = SidedInvWrapper.create(inventory, Direction.UP, Direction.DOWN);

    private int powerStored = 0;

    public BatteryBoxTile(BlockPos pos, BlockState state) {
        super(ExpansionReferences.BATTERY_BOX_TILE, pos, state);
        inventory.addListener(c -> setChanged());
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        tag.putInt("storage", powerStored);
        tag.put("inventory", inventory.createTag());
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        powerStored = tag.getInt("storage");
        inventory.fromTag(tag.getList("inventory", 10));
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
    }

    @Override
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        if (!getLevel().isClientSide) {
            ServerUtils.openContainer(
                    (ServerPlayer) player,
                    new SimpleMenuProvider(
                            (id, inv, p) -> new BatteryBoxContainer(inv, this, id),
                            new TextComponent(getBlockState().getBlock().getDescriptionId())),
                    p -> p.writePos(getBlockPos()));
        }

        return InteractionResult.sidedSuccess(getLevel().isClientSide);
    }

    @Override
    public void onBlockPlaced(LivingEntity player, ItemStack item) {
        super.onBlockPlaced(player, item);
        if (!getBlockLevel().isClientSide && item.hasTag() && item.getTag().contains(TAG_KEY_POWER_STORED)) {
            powerStored = item.getTag().getInt(TAG_KEY_POWER_STORED);
            pushBlockState();
        }
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        dropInventory(inventory, getLevel(), Vector3.fromBlockPos(getBlockPos()));
    }

    public ItemStack createStackWithStoredPower() {
        ItemStack stack = new ItemStack(BATTERY_BOX_BLOCK, 1);
        if (powerStored > 0) {
            CompoundTag tag = stack.getOrCreateTag();
            tag.putInt(TAG_KEY_POWER_STORED, powerStored);
            tag.putInt(TAG_KEY_CHARGE_LEVEL_STATE, getPowerStoredScaled(8));
        }
        return stack;
    }

    @Override
    public void tick() {
        super.tick();
        if (getLevel().isClientSide) return;

        boolean changed = false;

        // Attempt to keep conductor charge between UpperChargeTarget and LowerChargeTarget by
        // respectively drawing from or to internal storage
        if (getConductorCharge() > getConductorUpperChargeTarget() && powerStored < getMaxStorage()) {
            int n = Math.min(getConductorCharge() - getConductorUpperChargeTarget(), getConductorChargeSpeed()) / 10;
            n = Math.min(n, getMaxStorage() - powerStored);
            conductor.applyPower(n * -1000);
            powerStored += n;
            if (n != 0) changed = true;
        } else if (getConductorCharge() < getConductorLowerChargeTarget() && powerStored > 0) {
            int n = Math.min(getConductorLowerChargeTarget() - getConductorCharge(), getConductorChargeSpeed()) / 10;
            n = Math.min(n, powerStored);
            conductor.applyPower(n * 1000);
            powerStored -= n;
            if (n != 0) changed = true;
        }

        // Charge and discharge items in inventory
        changed |= tryChargeBattery();
        changed |= tryDischargeBattery();

        // Update block state if render level changed
        int prevPowerLevel = getBlockState().getValue(BatteryBoxBlock.CHARGE_LEVEL);
        int newPowerLevel = getPowerStoredScaled(8);
        if (prevPowerLevel != newPowerLevel) {
            pushBlockState();
        }

        // Mark chunk data changed
        if (changed) {
            setChanged();
        }
    }

    public int getMaxStorage() {
        return 8000;
    }

    public int getConductorUpperChargeTarget() {
        return 900;
    }

    public int getConductorLowerChargeTarget() {
        return 800;
    }

    protected int getConductorChargeSpeed() {
        return 100;
    }

    protected int getBatteryChargeSpeed() {
        return 25;
    }

    protected int getPowerStoredScaled(int i) {
        return Math.min(i, i * powerStored / getMaxStorage());
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(BatteryBoxBlock.CHARGE_LEVEL, getPowerStoredScaled(8));
    }

    private boolean tryChargeBattery() {
        ItemStack stack = inventory.getItem(0);
        if (!stack.isEmpty() && stack.getItem() instanceof IChargable) {
            int toAdd = Math.min(powerStored, getBatteryChargeSpeed());
            Tuple<ItemStack, Integer> result = ((IChargable) stack.getItem()).addPower(stack, toAdd);
            inventory.setItem(0, result.getA());
            powerStored -= result.getB();
            return result.getB() != 0;
        }
        return false;
    }

    private boolean tryDischargeBattery() {
        ItemStack stack = inventory.getItem(1);
        if (!stack.isEmpty() && stack.getItem() instanceof IChargable) {
            int toDraw = Math.min(getMaxStorage() - powerStored, getBatteryChargeSpeed());
            Tuple<ItemStack, Integer> result = ((IChargable) stack.getItem()).drawPower(stack, toDraw);
            inventory.setItem(1, result.getA());
            powerStored += result.getB();
            return result.getB() != 0;
        }
        return false;
    }

    //region Capabilities
    @Nonnull
    @Override
    public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
        if (!this.remove && cap == net.minecraftforge.items.CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
            return side == Direction.UP ? handlers[0].cast() : handlers[1].cast();
        }
        return super.getCapability(cap, side);
    }

    @Override
    public void invalidateCaps() {
        super.invalidateCaps();
        for (LazyOptional<?> handler : handlers) {
            handler.invalidate();
        }
    }
    //endregion

    //region Container getters
    public Container getInventory() {
        return inventory;
    }
    public int getPowerStored() {
        return powerStored;
    }
    //endregion

    private static class BatteryBoxInventory extends SimpleContainer implements WorldlyContainer {

        private static final int[] TOP_SLOTS = new int[]{0};
        private static final int[] BOTTOM_SLOTS = new int[]{1};

        public BatteryBoxInventory() {
            super(2);
        }

        @Override
        public int getMaxStackSize() {
            return 1;
        }

        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return stack.getItem() instanceof IRechargableBattery;
        }

        @Override
        public int[] getSlotsForFace(Direction direction) {
            return direction == Direction.UP ? TOP_SLOTS : BOTTOM_SLOTS;
        }

        @Override
        public boolean canPlaceItemThroughFace(int slot, ItemStack stack, @Nullable Direction direction) {
            return true;
        }

        @Override
        public boolean canTakeItemThroughFace(int slot, ItemStack stack, Direction direction) {
            return true;
        }
    }
}
