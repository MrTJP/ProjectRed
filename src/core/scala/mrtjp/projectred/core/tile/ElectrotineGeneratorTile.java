package mrtjp.projectred.core.tile;

import codechicken.lib.util.ServerUtils;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.*;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorContainer;
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

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ElectrotineGeneratorTile extends BasePoweredTile implements ILowLoadMachine {

    protected final JDrawPointPowerConductor conductor = new JDrawPointPowerConductor(this,
            IntStream.rangeClosed(0, 29)
                    .boxed()
                    .collect(Collectors.toList()));

    private final Inventory inventory = new Inventory(1) {
        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return stack.getItem() == CoreContent.itemElectrotineDust().get();
        }
    };

    private int burnTimeRemaining = 0;
    private int powerStored = 0;

    public ElectrotineGeneratorTile() {
        super(CoreContent.electrotineGeneratorTile().get());
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
                .setValue(ProjectRedBlock.CHARGED, conductor.canWork())
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

    @Nonnull
    @Override
    public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
        return super.getCapability(cap, side); //TODO add capabilities
    }

    @Override
    public void tick() {
        if (getLevel().isClientSide) return;

        conductor.update();

        tryBurnDust();
        tryChargeStorage();
        tryChargeConductor();

        if (level.getGameTime() % 10 == 0) updateRendersIfNeeded();
    }

    @Override
    public PowerConductor conductor(int dir) {
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
            burnTimeRemaining -= Math.max(burnTimeRemaining, getBurnUseOnIdle());
        }
    }

    private void tryChargeConductor() {
        if (conductor.charge() < getDrawFloor() && powerStored > 0) {
            int n = Math.min(getDrawFloor() - conductor.charge(), getDrawSpeed()) / 10;
            n = Math.min(n, powerStored);
            conductor.applyPower(n * 1000);
            powerStored -= n;
        }
    }

    private void updateRendersIfNeeded() {

        boolean lastWorking = getBlockState().getValue(ProjectRedBlock.WORKING);
        boolean lastCharged = getBlockState().getValue(ProjectRedBlock.CHARGED);
        boolean isWorking = burnTimeRemaining > 0;
        boolean isCharged = conductor.canWork();

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
        return 1000;
    }

    public int getMaxStorage() {
        return 800;
    }

    public int getMaxBurnTime() {
        return 2750;
    }

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

}
