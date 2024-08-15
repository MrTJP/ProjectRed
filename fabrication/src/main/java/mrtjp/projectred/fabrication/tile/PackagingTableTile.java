package mrtjp.projectred.fabrication.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.init.CoreItems;
import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.core.tile.IPacketReceiverTile;
import mrtjp.projectred.fabrication.engine.ICInterfaceType;
import mrtjp.projectred.fabrication.engine.InterfaceSpec;
import mrtjp.projectred.fabrication.init.FabricationBlocks;
import mrtjp.projectred.fabrication.inventory.container.PackagingTableContainer;
import mrtjp.projectred.fabrication.item.ValidDieItem;
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
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static mrtjp.projectred.fabrication.editor.EditorDataUtils.getInterfaceSpec;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.hasFabricationTarget;

public class PackagingTableTile extends FabricationMachineTile implements IPacketReceiverTile {

    private static final int DIE_SLOT = 4;
    private static final int OUTPUT_SLOT = 9;

    private final BaseInventory inventory = new BaseInventory(10) {

        @Override
        public boolean canPlaceItem(int slot, @NotNull ItemStack stack) {
            return switch (slot) {
                case DIE_SLOT -> stack.getItem() instanceof ValidDieItem;
                case OUTPUT_SLOT -> false;
                default -> true;
            };
        }

        @Override
        public void setChanged() {
            super.setChanged();
            cancelWorkIfNeeded();
        }
    };

    private int problematicSlotMask = 0; // Masks of slots that client should render red highlights

    public PackagingTableTile(BlockPos pos, BlockState state) {
        super(FabricationBlocks.PACKAGING_TABLE_TILE.get(), pos, state);
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
                    new SimpleMenuProvider((id, inv, p) -> new PackagingTableContainer(inv, this, id), getBlockState().getBlock().getName()),
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

        problematicSlotMask = 0;

        ItemStack dieItem = inventory.getItem(DIE_SLOT);
        CompoundTag dieTag = dieItem.getTag();

        if (dieItem.isEmpty() || !(dieItem.getItem() instanceof ValidDieItem) || !hasFabricationTarget(dieTag)) {
            problematicSlotMask |= 1 << DIE_SLOT;
            return false; // Fail-fast. Can't do ingredient checks without valid die item
        }

        if (!inventory.getItem(OUTPUT_SLOT).isEmpty()) {
            problematicSlotMask |= 1 << OUTPUT_SLOT;
        }

        // Check IO ingredients
        InterfaceSpec iospec = getInterfaceSpec(dieTag);
        int[] slotMap = { 1, 5, 7, 3 }; // Maps rotation to grid slot
        for (int r = 0; r < 4; r++) {
            ICInterfaceType type = iospec.getInterfaceType(r);

            // Each type of IO corresponds to a particular ingredient
            boolean match = inventory.getItem(slotMap[r]).is(switch (type) {
                case NC       -> CoreItems.PLATE_ITEM.get();
                case REDSTONE, ANALOG -> CoreItems.CONDUCTIVE_PLATE_ITEM.get();
                case BUNDLED  -> CoreItems.BUNDLED_PLATE_ITEM.get();
            });

            if (!match) {
                problematicSlotMask |= 1 << slotMap[r];
            }
        }

        // Check corner slots
        int[] cornerSlots = { 0, 2, 6, 8 };
        for (int slot : cornerSlots) {
            if (!inventory.getItem(slot).is(CoreItems.PLATE_ITEM.get())) {
                problematicSlotMask |= 1 << slot;
            }
        }

        return problematicSlotMask == 0;
    }

    @Override
    protected int startWork() {
        return 20 * 20; // 20 seconds worth of work
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
        ItemStack gatePart = ValidDieItem.createGatePart(inventory.getItem(DIE_SLOT));
        inventory.setItem(OUTPUT_SLOT, gatePart); //TODO or stack

        // Consume inputs
        for (int i = 0; i < 9; i++) {
            inventory.removeItem(i, 1);
        }
    }

    //region Container data
    public int getProblematicSlotMask() {
        return problematicSlotMask;
    }
    //endregion
}
