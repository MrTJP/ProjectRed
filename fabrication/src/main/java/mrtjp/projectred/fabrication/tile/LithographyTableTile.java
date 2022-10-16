package mrtjp.projectred.fabrication.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.fabrication.inventory.container.LithographyTableContainer;
import mrtjp.projectred.fabrication.item.BaseSiliconWaferItem;
import mrtjp.projectred.fabrication.item.PhotomaskSetItem;
import mrtjp.projectred.fabrication.lithography.ProcessNode;
import mrtjp.projectred.fabrication.lithography.WaferType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.TranslatableComponent;
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

public class LithographyTableTile extends FabricationMachineTile {

    private final BaseInventory inventory = new BaseInventory(4) {

        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            switch (slot) {
                case 0:
                    return stack.getItem() instanceof PhotomaskSetItem;
                case 1:
                    return stack.getItem() instanceof BaseSiliconWaferItem;
                default:
                    return false;
            }
        }

        @Override
        public void setChanged() {
            super.setChanged();
            cancelWorkIfNeeded();
        }
    };

    public LithographyTableTile(BlockPos pos, BlockState state) {
        super(FabricationReferences.LITHOGRAPHY_TABLE_TILE, pos, state);
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
        inventory.fromTag(tag.getList("inventory", 10));
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
            ServerUtils.openContainer((ServerPlayer) player, new SimpleMenuProvider((id, inv, p) -> new LithographyTableContainer(inv, this, id), new TranslatableComponent(getBlockState().getBlock().getDescriptionId())), p -> p.writePos(getBlockPos()));
        }

        return InteractionResult.SUCCESS;
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

        if (!(slot0.getItem() instanceof PhotomaskSetItem)) return false;
        if (!(slot1.getItem() instanceof BaseSiliconWaferItem)) return false;

        return inventory.getItem(2).isEmpty() && inventory.getItem(3).isEmpty();
    }

    @Override
    protected int startWork() {
        return 20 * 60; // 60 seconds worth of work
    }

    @Override
    protected int tickWork(int remainingWork) {
        if (canConductorWork()) {
            conductor.applyPower(-1100); // draw at rate of 1.1kW
            return 1;
        }
        return 0;
    }

    @Override
    protected void finishWork() {
        BaseSiliconWaferItem wafer = (BaseSiliconWaferItem) inventory.getItem(1).getItem();

        WaferType waferType = wafer.getWaferType();
        ProcessNode processNode = ProcessNode.PROCESS_64NM;
        int dieLen = processNode.getTileLen() * 16; //TODO Source from photomask itemstack instead of assuming 16x16
        int gridLen = waferType.getWaferLen() / dieLen;
        double defectChancePerDie = dieLen * waferType.getDefectChancePerLen();

        // Calculate number of good and bad dies
        int totalDesigns = gridLen * gridLen;
        int totalDefectiveDies = 0;
        for (int i = 0; i < totalDesigns; i++) {
            if (Math.random() < defectChancePerDie) {
                totalDefectiveDies++;
            }
        }
        int totalValidDies = totalDesigns - totalDefectiveDies;

        // Set the output stacks
        if (totalValidDies > 0) {
            ItemStack validDieStack = PhotomaskSetItem.createDieStack(inventory.getItem(0), totalValidDies);
            inventory.setItem(2, validDieStack);
        }
        if (totalDefectiveDies > 0) {
            ItemStack defectiveDieStack = new ItemStack(FabricationReferences.INVALID_DIE_ITEM, totalDefectiveDies);
            inventory.setItem(3, defectiveDieStack);
        }

        // Consume inputs
        inventory.removeItem(1, 1); // Consume wafer
    }
}
