package mrtjp.projectred.core.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.core.tile.ElectrotineGeneratorTile;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;

import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_DUST_ITEM;
import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_GENERATOR_CONTAINER;

public class ElectrotineGeneratorContainer extends BasePoweredTileContainer {

    public static ICCLContainerFactory<ElectrotineGeneratorContainer> FACTORY = (windowId, inventory, packet) -> {
        TileEntity tile = inventory.player.level.getBlockEntity(packet.readPos());
        if (!(tile instanceof ElectrotineGeneratorTile)) return null;

        return new ElectrotineGeneratorContainer(inventory, (ElectrotineGeneratorTile) tile, windowId);
    };

    private final PlayerInventory playerInventory;
    private final ElectrotineGeneratorTile tile;

    private int burnTimeRemaining = 0;
    private int powerStored = 0;

    public ElectrotineGeneratorContainer(PlayerInventory playerInventory, ElectrotineGeneratorTile tile, int windowId) {
        super(ELECTROTINE_GENERATOR_CONTAINER, windowId, tile);

        this.playerInventory = playerInventory;
        this.tile = tile;

        InventoryLib.addPlayerInventory(playerInventory, 8, 89, this::addSlot);
        addElectrotineGeneratorInventory();
    }

    private void addElectrotineGeneratorInventory() {
        addSlot(new Slot(tile.getInventory(), 0, 134, 42) {
            @Override
            public boolean mayPlace(ItemStack stack) {
                return stack.getItem() == ELECTROTINE_DUST_ITEM;
            }
        });
    }

    @Override
    public void broadcastChanges() {
        super.broadcastChanges();

        boolean needsBurnTimeRemaining = burnTimeRemaining != tile.getBurnTimeRemaining();
        boolean needsPowerStored = powerStored != tile.getPowerStored();

        burnTimeRemaining = tile.getBurnTimeRemaining();
        powerStored = tile.getPowerStored();

        for (IContainerListener listener : containerListeners) {
            if (needsBurnTimeRemaining) {
                listener.setContainerData(this, 110, burnTimeRemaining);
            }
            if (needsPowerStored) {
                listener.setContainerData(this, 111, powerStored);
            }
        }
    }

    @Override
    public void setData(int id, int value) {
        switch (id) {
            case 110:
                burnTimeRemaining = value;
                break;
            case 111:
                powerStored = value;
                break;
            default:
                super.setData(id, value);
        }
    }

    @Override
    public ItemStack quickMoveStack(PlayerEntity player, int slotIndex) {

        Slot slot = slots.get(slotIndex);
        if (slot == null || !slot.hasItem()) return ItemStack.EMPTY;

        ItemStack stack = slot.getItem();
        ItemStack originalStack = stack.copy();

        if (isFuel(slotIndex)) {
            if (!moveToEntireInventory(stack, false)) return ItemStack.EMPTY;

        } else if (stack.getItem() == ELECTROTINE_DUST_ITEM) {
            if (!moveToFuel(stack, false)) return ItemStack.EMPTY;

        } else if (isPlayerInventory(slotIndex)) {
            if (!moveToHotbar(stack, false)) return ItemStack.EMPTY;

        } else { //Hotbar
            if (!moveToPlayerInventory(stack, false)) return ItemStack.EMPTY;
        }

        if (stack.isEmpty()) {
            slot.set(ItemStack.EMPTY);
        } else {
            slot.setChanged();
        }

        if (stack.getCount() == originalStack.getCount()) {
            return ItemStack.EMPTY;
        }

        slot.onTake(player, stack);
        return originalStack;
    }

    public int getBurnTimeScaled(int scale) {
        return burnTimeRemaining == 0 ? 0 : scale * burnTimeRemaining / tile.getMaxBurnTime();
    }

    public int getPowerStoredScaled(int scale) {
        return powerStored == 0 ? 0 : scale * powerStored / tile.getMaxStorage();
    }

    public boolean isPowerStorageFull() {
        return powerStored == tile.getMaxStorage();
    }

    public boolean isBurning() {
        return burnTimeRemaining > 0;
    }

    public boolean isChargingStorage() {
        return burnTimeRemaining > tile.getBurnUseOnCharge() && powerStored < tile.getMaxStorage();
    }

    public boolean isChargingConductor() {
        return condCharge < tile.getDrawFloor() && (powerStored > 0 || burnTimeRemaining > tile.getBurnUseOnCharge());
    }

    //@formatter:off
    private boolean isPlayerInventory(int slotIndex) {
        return slotIndex >= 0 && slotIndex < 27;
    }
    private boolean isHotbar(int slotIndex) {
        return slotIndex >= 27 && slotIndex < 36;
    }
    private boolean isFuel(int slotIndex) {
        return slotIndex == 36;
    }

    private boolean moveToPlayerInventory(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 0, 27, reverse);
    }
    private boolean moveToHotbar(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 27, 36, reverse);
    }
    private boolean moveToEntireInventory(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 0, 36, reverse);
    }
    private boolean moveToFuel(ItemStack stack, boolean reverse) {
        return moveItemStackTo(stack, 36, 37, reverse);
    }
    //@formatter:on
}
