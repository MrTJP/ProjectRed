package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.inventory.container.CCLMenuType;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.inventory.BaseContainer;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.inventory.container.ChargingBenchMenu;
import mrtjp.projectred.expansion.item.IChargable;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Tuple;
import net.minecraft.world.*;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.wrapper.SidedInvWrapper;

import javax.annotation.Nullable;

public class ChargingBenchBlockEntity extends LowLoadPoweredBlockEntity {

    private final ChargingBenchInventory inventory = new ChargingBenchInventory();

    private final IItemHandler[] handlers = {
            new SidedInvWrapper(inventory, Direction.DOWN),
            new SidedInvWrapper(inventory, Direction.UP)
    };

    private boolean isCharged = false;
    private int chargeSlot = 0;
    private int powerStored = 0;

    public ChargingBenchBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.CHARGING_BENCH_BLOCK_ENTITY.get(), pos, state);
        inventory.addListener(c -> setChanged());
    }

    @Override
    public void saveToNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.saveToNBT(tag, lookupProvider);
        tag.putInt("storage", powerStored);
        tag.putByte("chargeSlot", (byte) chargeSlot);
        inventory.saveTo(tag, "inventory", lookupProvider);
    }

    @Override
    public void loadFromNBT(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        super.loadFromNBT(tag, lookupProvider);
        powerStored = tag.getInt("storage");
        chargeSlot = tag.getByte("chargeSlot") & 0xFF;
        inventory.loadFrom(tag, "inventory", lookupProvider);
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
    public InteractionResult useWithoutItem(Player player, BlockHitResult hit) {
        if (!getLevel().isClientSide) {
            CCLMenuType.openMenu(
                    (ServerPlayer) player,
                    new SimpleMenuProvider(
                            (id, inv, p) -> new ChargingBenchMenu(inv, this, id),
                            getBlockState().getBlock().getName()),
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
    public IItemHandler getHandler(Direction side) {
        return handlers[side == Direction.UP ? 1 : 0];
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

    private static class ChargingBenchInventory extends BaseContainer implements WorldlyContainer {

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
