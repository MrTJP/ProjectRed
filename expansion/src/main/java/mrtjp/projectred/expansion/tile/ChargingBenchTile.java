package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import mrtjp.projectred.expansion.inventory.container.ChargingBenchContainer;
import mrtjp.projectred.expansion.item.IChargable;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Tuple;
import net.minecraft.world.*;
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

public class ChargingBenchTile extends LowLoadPoweredTile {

    private final ChargingBenchInventory inventory = new ChargingBenchInventory();
    private final LazyOptional<? extends IItemHandler>[] handlers = SidedInvWrapper.create(inventory, Direction.UP, Direction.DOWN);

    private boolean isCharged = false;
    private int chargeSlot = 0;
    private int powerStored = 0;

    public ChargingBenchTile(BlockPos pos, BlockState state) {
        super(ExpansionReferences.CHARGING_BENCH_TILE, pos, state);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        tag.putInt("storage", powerStored);
        tag.putByte("chargeSlot", (byte) chargeSlot);
        tag.put("inventory", inventory.createTag());
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        powerStored = tag.getInt("storage");
        chargeSlot = tag.getByte("chargeSlot") & 0xFF;
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
                            (id, inv, p) -> new ChargingBenchContainer(inv, this, id),
                            new TextComponent(getBlockState().getBlock().getDescriptionId())),
                    p -> p.writePos(getBlockPos()));
        }

        return InteractionResult.sidedSuccess(level.isClientSide);
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        dropInventory(inventory, getLevel(), Vector3.fromBlockPos(getBlockPos()));
    }

    @Override
    public void tick() {
        super.tick();
        if (getLevel().isClientSide) return;
        isCharged = canConductorWork();

        boolean changed = false;

        // Skim power from conductor if it is above the upper charge target to fill internal storage
        if (getConductorCharge() > getConductorUpperChargeTarget() && powerStored < getMaxStorage()) {
            int n = Math.min(getConductorCharge() - getConductorUpperChargeTarget(), getConductorChargeSpeed()) / 10;
            n = Math.min(n, getMaxStorage() - powerStored);
            conductor.applyPower(n * -10);
            powerStored += n;
            if (n != 0) changed = true;
        }

        // Charge items in inventory
        changed |= tryChargeItemInSlot(chargeSlot);
        changed |= tryDropDownItemInSlot(chargeSlot);
        chargeSlot = (chargeSlot + 1) % 8;

        // Update block state if needed
        BlockState state = getBlockState();
        if (isCharged != state.getValue(ProjectRedBlock.CHARGED)) {
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
        return 600;
    }

    protected int getConductorChargeSpeed() {
        return 100;
    }

    protected int getChargeSpeed() {
        return 15;
    }

    protected int getPowerStoredScaled(int i) {
        return Math.min(i, i * powerStored / getMaxStorage());
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(ProjectRedBlock.CHARGED, isCharged);
    }

    private boolean tryChargeItemInSlot(int i) {
        ItemStack stack = inventory.getItem(i);
        if (!stack.isEmpty() && stack.getItem() instanceof IChargable) {
            int toAdd = Math.min(powerStored, getChargeSpeed());
            Tuple<ItemStack, Integer> result = ((IChargable) stack.getItem()).addPower(stack, toAdd);
            inventory.setItem(i, result.getA());
            powerStored -= result.getB();
            return result.getB() != 0;
        }
        return false;
    }

    private boolean tryDropDownItemInSlot(int i) {
        ItemStack stack = inventory.getItem(i);
        if (!stack.isEmpty() && stack.getItem() instanceof IChargable) {
            if (((IChargable) stack.getItem()).isFullyCharged(stack)) {
                InventoryLib.injectItemStack(inventory, stack, 8, 16, false);
                inventory.setItem(i, stack.isEmpty() ? ItemStack.EMPTY : stack);
                return true;
            }
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

    private static class ChargingBenchInventory extends SimpleContainer implements WorldlyContainer {

        private static final int[] TOP_SLOTS = new int[8];
        private static final int[] BOTTOM_SLOTS = new int[8];

        static {
            for (int i = 0; i < 8; i++) {
                TOP_SLOTS[i] = i;
                BOTTOM_SLOTS[i] = i + 8;
            }
        }

        public ChargingBenchInventory() {
            super(16);
        }

        @Override
        public int getMaxStackSize() {
            return 1;
        }

        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return stack.getItem() instanceof IChargable;
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
