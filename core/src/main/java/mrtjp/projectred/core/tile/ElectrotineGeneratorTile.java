package mrtjp.projectred.core.tile;

import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorContainer;
import mrtjp.projectred.core.power.ILowLoadMachine;
import mrtjp.projectred.core.power.ILowLoadPowerLine;
import mrtjp.projectred.core.power.PowerConductor;
import net.minecraft.block.BlockState;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.wrapper.InvWrapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_DUST_ITEM;
import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_GENERATOR_TILE;

public class ElectrotineGeneratorTile extends BasePoweredTile implements ILowLoadMachine {

    protected final PowerConductor conductor = new PowerConductor(this, 0.01, 160);

    private int chargeFlow = 0;

    private final ElectrotineGeneratorInventory inventory = new ElectrotineGeneratorInventory();
    private final LazyOptional<? extends IItemHandler> handler = LazyOptional.of(() -> new InvWrapper(inventory));

    private int burnTimeRemaining = 0;
    private int powerStored = 0;

    public ElectrotineGeneratorTile() {
        super(ELECTROTINE_GENERATOR_TILE);
    }

    @Override
    public void saveToNBT(CompoundNBT tag) {
        super.saveToNBT(tag);
        conductor.save(tag);
        tag.put("inventory", inventory.createTag());
        tag.putInt("burnTime", burnTimeRemaining);
        tag.putInt("stored", powerStored);
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
        super.loadFromNBT(tag);
        conductor.load(tag);
        inventory.fromTag(tag.getList("inventory", 10));
        burnTimeRemaining = tag.getInt("burnTime");
        powerStored = tag.getInt("stored");
    }

    @Override
    public void loadBlockState(BlockState state) {
        super.loadBlockState(state);
    }

    @Override
    public BlockState storeBlockState(BlockState defaultState) {
        return super.storeBlockState(defaultState)
                .setValue(ProjectRedBlock.CHARGED, canConductorWork())
                .setValue(ProjectRedBlock.WORKING, burnTimeRemaining > 0);
    }

    @Override
    public ActionResultType onBlockActivated(PlayerEntity player, Hand hand, BlockRayTraceResult hit) {
        if (!getLevel().isClientSide) {
            ServerUtils.openContainer((ServerPlayerEntity) player, new SimpleNamedContainerProvider(
                            (id, inv, p) -> new ElectrotineGeneratorContainer(inv, this, id),
                            new TranslationTextComponent(getBlockState().getBlock().getDescriptionId())),
                    p -> p.writePos(getBlockPos()));
        }

        return ActionResultType.SUCCESS;
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
    @Nonnull
    @Override
    public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
        if (!this.remove && cap == net.minecraftforge.items.CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
            return handler.cast();
        }
        return super.getCapability(cap, side);
    }

    @Override
    protected void invalidateCaps() {
        super.invalidateCaps();
        handler.invalidate();
    }
    //endregion

    //region Container getters
    public Inventory getInventory() {
        return inventory;
    }

    public int getBurnTimeRemaining() {
        return burnTimeRemaining;
    }

    public int getPowerStored() {
        return powerStored;
    }
    //endregion

    private static class ElectrotineGeneratorInventory extends Inventory {

        public ElectrotineGeneratorInventory() {
            super(1);
        }

        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return stack.getItem() == ELECTROTINE_DUST_ITEM;
        }
    }
}
