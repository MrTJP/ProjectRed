package mrtjp.projectred.expansion.tile;

import codechicken.lib.util.ServerUtils;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.block.ProjectRedBlock;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;

public abstract class BaseMachineTile extends LowLoadPoweredTile {

    private boolean isWorking = false;
    private boolean isCharged = false;
    private int remainingWork = 0;
    private int totalWork = 0;

    public BaseMachineTile(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        tag.putInt("remaining_work", remainingWork);
        tag.putInt("total_work", totalWork);
        tag.putBoolean("working", isWorking);
        tag.putBoolean("charging", isCharged);
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
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

        isCharged = canConductorWork();
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
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        ItemStack held = player.getItemInHand(hand);

        // Try to rotate block
        if (held.getItem() instanceof IScrewdriver) {
            IScrewdriver screwdriver = (IScrewdriver) held.getItem();

            if (screwdriver.canUse(player, held)) {
                if (!level.isClientSide) {
                    rotateBlock();
                    screwdriver.damageScrewdriver(player, held);
                }
                return InteractionResult.sidedSuccess(level.isClientSide);
            }
        }

        // Otherwise, open GUI
        if (!level.isClientSide) {
            openGui(player);
        }
        return InteractionResult.sidedSuccess(level.isClientSide);
    }

    private void rotateBlock() {
        BlockState state = getLevel().getBlockState(getBlockPos());
        int r = state.getValue(ProjectRedBlock.ROTATION);
        int newRotation = (r + 1) % 4;
        getLevel().setBlockAndUpdate(getBlockPos(), state.setValue(ProjectRedBlock.ROTATION, newRotation));
    }

    private void openGui(Player player) {
        ServerUtils.openContainer(
                (ServerPlayer) player,
                new SimpleMenuProvider(
                        this::createMenu,
                        new TextComponent(getBlockState().getBlock().getDescriptionId())),
                p -> p.writePos(getBlockPos()));
    }

    protected abstract AbstractContainerMenu createMenu(int windowId, Inventory playerInventory, Player player);

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
