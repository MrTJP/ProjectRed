package mrtjp.projectred.expansion.tile;

import codechicken.lib.util.ServerUtils;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.block.ProjectRedBlock;
import net.minecraft.block.BlockState;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.TileEntityType;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.text.TranslationTextComponent;

public abstract class BaseMachineTile extends LowLoadPoweredTile {

    private boolean isWorking = false;
    private boolean isCharged = false;
    private int remainingWork = 0;
    private int totalWork = 0;

    public BaseMachineTile(TileEntityType<?> type) {
        super(type);
    }

    @Override
    public void saveToNBT(CompoundNBT tag) {
        super.saveToNBT(tag);
        tag.putInt("remaining_work", remainingWork);
        tag.putInt("total_work", totalWork);
        tag.putBoolean("working", isWorking);
        tag.putBoolean("charging", isCharged);
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
        super.loadFromNBT(tag);
        remainingWork = tag.getInt("remaining_work");
        totalWork = tag.getInt("total_work");
        isWorking = tag.getBoolean("working");
        isCharged = tag.getBoolean("charging");
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(ProjectRedBlock.CHARGED, isCharged)
                .setValue(ProjectRedBlock.WORKING, isWorking);
    }

    @Override
    public void tick() {
        super.tick();

        if (getLevel().isClientSide) return;

        isCharged = conductor.canWork();
        if (!isWorking) { // Not currently working
            // Try to start work
            if (canStartOrContinueWork()) {
                isWorking = true;
                remainingWork = totalWork = startWork();
            }

        } if (!canStartOrContinueWork()) { // Currently working but cant continue
            // Stop work
            isWorking = false;
            remainingWork = 0;
            totalWork = 0;

        } else { // Working normally

            // Tick work
            int workDone = tickWork(remainingWork);
            remainingWork -= workDone;

            // Complete work
            if (remainingWork <= 0) {
                isWorking = false;
                remainingWork = 0;
                totalWork = 0;
                finishWork();
            }
        }

        // Update block state if needed
        BlockState state = getBlockState();
        if (isCharged != state.getValue(ProjectRedBlock.CHARGED) ||
                isWorking != state.getValue(ProjectRedBlock.WORKING)) {
            pushBlockState();
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
                    rotateBlock();
                    screwdriver.damageScrewdriver(player, held);
                }
                return ActionResultType.sidedSuccess(level.isClientSide);
            }
        }

        // Otherwise, open GUI
        if (!level.isClientSide) {
            openGui(player);
        }
        return ActionResultType.sidedSuccess(level.isClientSide);
    }

    private void rotateBlock() {
        BlockState state = getLevel().getBlockState(getBlockPos());
        int r = state.getValue(ProjectRedBlock.ROTATION);
        int newRotation = (r + 1) % 4;
        getLevel().setBlockAndUpdate(getBlockPos(), state.setValue(ProjectRedBlock.ROTATION, newRotation));
    }

    private void openGui(PlayerEntity player) {
        ServerUtils.openContainer(
                (ServerPlayerEntity) player,
                new SimpleNamedContainerProvider(
                        this::createMenu,
                        new TranslationTextComponent(getBlockState().getBlock().getDescriptionId())),
                p -> p.writePos(getBlockPos()));
    }

    protected abstract Container createMenu(int windowId, PlayerInventory playerInventory, PlayerEntity player);

    public int getRemainingWork() {
        return remainingWork;
    }

    public int getTotalWork() {
        return totalWork;
    }

    protected abstract boolean canStartOrContinueWork();
    protected abstract int startWork();
    protected abstract int tickWork(int remainingWork);
    protected abstract void finishWork();
}
