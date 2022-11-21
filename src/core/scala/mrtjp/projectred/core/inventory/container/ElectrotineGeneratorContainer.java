package mrtjp.projectred.core.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.core.CoreContent;
import mrtjp.projectred.core.tile.ElectrotineGeneratorTile;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;

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
        super(CoreContent.electrotineGeneratorContainer().get(), windowId, tile);

        this.playerInventory = playerInventory;
        this.tile = tile;

        addPlayerInventory(playerInventory, 8, 89);
        addElectrotineGeneratorInventory();
    }

    private void addElectrotineGeneratorInventory() {
        addSlot(new Slot(tile.getInventory(), 0, 134, 42) {
            @Override
            public boolean mayPlace(ItemStack stack) {
                return stack.getItem() == CoreContent.itemElectrotineDust().get();
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
}
