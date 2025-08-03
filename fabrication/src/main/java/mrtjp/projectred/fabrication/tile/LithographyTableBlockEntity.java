package mrtjp.projectred.fabrication.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.inventory.BaseContainer;
import mrtjp.projectred.fabrication.editor.EditorDataUtils;
import mrtjp.projectred.fabrication.init.FabricationBlocks;
import mrtjp.projectred.fabrication.init.FabricationItems;
import mrtjp.projectred.fabrication.inventory.container.LithographyTableMenu;
import mrtjp.projectred.fabrication.item.BaseSiliconWaferItem;
import mrtjp.projectred.fabrication.item.PhotomaskSetItem;
import mrtjp.projectred.fabrication.lithography.ProcessNode;
import mrtjp.projectred.fabrication.lithography.WaferType;
import net.minecraft.core.BlockPos;
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

public class LithographyTableBlockEntity extends FabricationMachineBlockEntity {

    private final BaseContainer inventory = new BaseContainer(4) {

        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return switch (slot) {
                case 0 -> stack.getItem() instanceof PhotomaskSetItem;
                case 1 -> stack.getItem() instanceof BaseSiliconWaferItem;
                default -> false;
            };
        }
    };

    public LithographyTableBlockEntity(BlockPos pos, BlockState state) {
        super(FabricationBlocks.LITHOGRAPHY_TABLE_BLOCK_ENTITY.get(), pos, state);
        inventory.addListener(this::onInventoryChanged);
    }

    public Container getInventory() {
        return inventory;
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        inventory.saveTo(tag, "inventory");;
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
                    new SimpleMenuProvider((id, inv, p) -> new LithographyTableMenu(inv, this, id), getBlockState().getBlock().getName()),
                    p -> p.writePos(getBlockPos()));
        }

        return InteractionResult.sidedSuccess(getLevel().isClientSide);
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        dropInventory(inventory, getLevel(), Vector3.fromBlockPos(getBlockPos()));
    }

    private void onInventoryChanged(Container inventory) {
        cancelWorkIfNeeded();
        setChanged();
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

        if (!EditorDataUtils.canFabricate(slot0.getTag())) return false;

        return inventory.getItem(2).isEmpty() && inventory.getItem(3).isEmpty();
    }

    @Override
    protected int startWork() {
        return 20 * 60; // 60 seconds worth of work
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
        BaseSiliconWaferItem wafer = (BaseSiliconWaferItem) inventory.getItem(1).getItem();

        WaferType waferType = wafer.getWaferType();
        ProcessNode processNode = ProcessNode.PROCESS_64NM;
        int dieWidth = processNode.getTileWidth() * 16; //TODO Source from photomask itemstack instead of assuming 16x16
        int dieHeight = processNode.getTileHeight() * 16;
        int gridWidth = waferType.getWaferWidth() / dieWidth;
        int gridHeight = waferType.getWaferHeight() / dieHeight;
        double defectChancePerDie = dieWidth * dieHeight * waferType.getDefectRatePerUnitArea();

        // Calculate number of good and bad dies
        int totalDesigns = gridWidth * gridHeight;
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
            ItemStack defectiveDieStack = new ItemStack(FabricationItems.INVALID_DIE_ITEM.get(), totalDefectiveDies);
            inventory.setItem(3, defectiveDieStack);
        }

        // Consume inputs
        inventory.removeItem(1, 1); // Consume wafer
    }
}
