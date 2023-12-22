package mrtjp.projectred.fabrication.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.fabrication.editor.EditorDataUtils;
import mrtjp.projectred.fabrication.init.FabricationBlocks;
import mrtjp.projectred.fabrication.inventory.container.PlottingTableContainer;
import mrtjp.projectred.fabrication.item.BlankPhotomaskItem;
import mrtjp.projectred.fabrication.item.ICBlueprintItem;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Container;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class PlottingTableTile extends FabricationMachineTile {

    private final BaseInventory inventory = new BaseInventory(3) {

        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return switch (slot) {
                case 0 -> stack.getItem() instanceof ICBlueprintItem;
                case 1 -> stack.getItem() instanceof BlankPhotomaskItem;
                default -> false;
            };
        }

        @Override
        public void setChanged() {
            super.setChanged();
            cancelWorkIfNeeded();
        }
    };

    public PlottingTableTile(BlockPos pos, BlockState state) {
        super(FabricationBlocks.PLOTTING_TABLE_TILE.get(), pos, state);
        inventory.addListener(c -> setChanged());
    }

    public Container getInventory() {
        return inventory;
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        tag.put("inventory", inventory.createTag());
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        inventory.loadFrom(tag, "inventory");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
    }

    @Override
    public void readDesc(MCDataInput in) {
    }

    @Override
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        if (!getLevel().isClientSide) {
            ServerUtils.openContainer((ServerPlayer) player,
                    new SimpleMenuProvider((id, inv, p) -> new PlottingTableContainer(inv, this, id), getBlockState().getBlock().getName()),
                    p -> p.writePos(getBlockPos()));
        }

        return InteractionResult.SUCCESS;
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        dropInventory(inventory, getLevel(), Vector3.fromBlockPos(getBlockPos()));
    }

    @Nonnull
    @Override
    public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
        return super.getCapability(cap, side); //TODO add capabilities
    }

    @Override
    protected boolean canStartWork() {

        ItemStack slot0 = inventory.getItem(0);
        ItemStack slot1 = inventory.getItem(1);

        if (slot0.isEmpty() || slot1.isEmpty()) {
            return false;
        }

        if (!(slot0.getItem() instanceof ICBlueprintItem)) return false;

        if (!(slot1.getItem() instanceof BlankPhotomaskItem)) return false;

        if (!EditorDataUtils.canFabricate(slot0.getTag())) return false;

        return inventory.getItem(2).isEmpty();
    }

    @Override
    protected int startWork() {
        return 20 * 10; // 10 seconds worth of work
    }

    @Override
    protected int tickWork(int remainingWork) {
        if (canConductorWork()) {
            conductor.applyPower(-100); // draw at rate of 100W
            return 1;
        }
        return 0;
    }

    @Override
    protected void finishWork() {
        ItemStack output = ICBlueprintItem.createPhotomaskStack(inventory.getItem(0));
        inventory.setItem(2, output);
        inventory.removeItem(1, 1); // delete 1 blank photomask
    }
}
