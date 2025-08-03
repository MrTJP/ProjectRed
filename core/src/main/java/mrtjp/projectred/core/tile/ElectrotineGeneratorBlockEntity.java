package mrtjp.projectred.core.tile;

import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.inventory.BaseContainer;
import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorMenu;
import mrtjp.projectred.core.power.ILowLoadMachine;
import mrtjp.projectred.core.power.ILowLoadPowerLine;
import mrtjp.projectred.core.power.PowerConductor;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.wrapper.InvWrapper;

import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK_ENTITY;
import static mrtjp.projectred.core.init.CoreItems.ELECTROTINE_DUST_ITEM;

public class ElectrotineGeneratorBlockEntity extends BasePoweredBlockEntity implements ILowLoadMachine {

    protected final PowerConductor conductor = new PowerConductor(this, 0.01, 160);

    private int chargeFlow = 0;

    private final ElectrotineGeneratorInventory inventory = new ElectrotineGeneratorInventory();
    private final IItemHandler handler = new InvWrapper(inventory);

    private int burnTimeRemaining = 0;
    private int powerStored = 0;

    public ElectrotineGeneratorBlockEntity(BlockPos pos, BlockState state) {
        super(ELECTROTINE_GENERATOR_BLOCK_ENTITY.get(), pos, state);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        conductor.save(tag);
        inventory.saveTo(tag, "inventory");
        tag.putInt("burnTime", burnTimeRemaining);
        tag.putInt("stored", powerStored);
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        conductor.load(tag);
        inventory.loadFrom(tag, "inventory");
        burnTimeRemaining = tag.getInt("burnTime");
        powerStored = tag.getInt("stored");
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(ProjectRedBlock.CHARGED, canConductorWork())
                .setValue(ProjectRedBlock.WORKING, burnTimeRemaining > 0);
    }

    @Override
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        if (!getLevel().isClientSide) {
            ServerUtils.openContainer((ServerPlayer) player, new SimpleMenuProvider(
                            (id, inv, p) -> new ElectrotineGeneratorMenu(inv, this, id),
                            getBlockState().getBlock().getName()),
                    p -> p.writePos(getBlockPos()));
        }

        return InteractionResult.sidedSuccess(getLevel().isClientSide);
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        dropInventory(inventory, getLevel(), Vector3.fromBlockPos(getBlockPos()));
    }

    @Override
    public void tick() {
        if (getLevel().isClientSide) return;

        conductor.tick();

        chargeFlow <<= 1;
        if (canConductorWork()) chargeFlow |= 1;

        tryBurnDust();
        tryChargeStorage();
        tryChargeConductor();

        if (level.getGameTime() % 10 == 0) updateRendersIfNeeded();
    }

    @Override
    public PowerConductor getConductor(int dir) {
        return conductor;
    }

    @Override
    public boolean canConnectPart(IConnectable part, int s, int edgeRot) {
        if (part instanceof ILowLoadPowerLine) return true;
        if (part instanceof ILowLoadMachine) return true;
        return false;
    }

    private void tryBurnDust() {
        if (powerStored < getMaxStorage() && burnTimeRemaining < getBurnUseOnCharge()) { // more burn required to charge
            ItemStack removedDust = inventory.removeItem(0, 1);
            if (!removedDust.isEmpty()) {
                int addedBurnTime = getBurnTimeForDust(removedDust);
                burnTimeRemaining = Math.min(getMaxBurnTime(), burnTimeRemaining + addedBurnTime);
            }
        }
    }

    private void tryChargeStorage() {
        if (burnTimeRemaining == 0) return;

        if (powerStored < getMaxStorage() && burnTimeRemaining >= getBurnUseOnCharge()) {
            powerStored += 1;
            burnTimeRemaining -= getBurnUseOnCharge();
        } else {
            burnTimeRemaining -= Math.min(burnTimeRemaining, getBurnUseOnIdle());
        }
    }

    private void tryChargeConductor() {
        if (getConductorCharge() < getDrawFloor() && powerStored > 0) {
            int n = Math.min(getDrawFloor() - getConductorCharge(), getDrawSpeed()) / 10;
            n = Math.min(n, powerStored);
            conductor.applyPower(n * 1000);
            powerStored -= n;
        }
    }

    private void updateRendersIfNeeded() {

        boolean lastWorking = getBlockState().getValue(ProjectRedBlock.WORKING);
        boolean lastCharged = getBlockState().getValue(ProjectRedBlock.CHARGED);
        boolean isWorking = burnTimeRemaining > 0;
        boolean isCharged = canConductorWork();

        if (lastWorking != isWorking || lastCharged != isCharged) {
            pushBlockState();
        }
    }

    protected int getBurnTimeForDust(ItemStack dust) {
        return 2750;
    }

    public int getBurnUseOnCharge() {
        return 10;
    }

    public int getBurnUseOnIdle() {
        return 1;
    }

    public int getDrawSpeed() {
        return 100;
    }

    public int getDrawFloor() {
        return 1200;
    }

    public int getMaxStorage() {
        return 800;
    }

    public int getMaxBurnTime() {
        return 2750;
    }

    @Override
    public int getConductorCharge() {
        return (int) (conductor.getVoltage() * 10);
    }

    @Override
    public int getConductorFlow() {
        return chargeFlow;
    }

    @Override
    public boolean canConductorWork() {
        return getConductorCharge() > 600;
    }

    //region Capabilities
    public IItemHandler getHandler() {
        return handler;
    }
    //endregion

    //region Container getters
    public SimpleContainer getInventory() {
        return inventory;
    }

    public int getBurnTimeRemaining() {
        return burnTimeRemaining;
    }

    public int getPowerStored() {
        return powerStored;
    }
    //endregion

    private static class ElectrotineGeneratorInventory extends BaseContainer {

        public ElectrotineGeneratorInventory() {
            super(1);
        }

        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return stack.getItem() == ELECTROTINE_DUST_ITEM.get();
        }
    }
}
