package mrtjp.projectred.exploration.inventory.container;

import codechicken.lib.inventory.container.ICCLContainerFactory;
import mrtjp.projectred.exploration.inventory.BackpackInventory;
import mrtjp.projectred.exploration.item.BackpackItem;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.item.ItemStack;

import static mrtjp.projectred.exploration.init.ExplorationReferences.BACKPACK_CONTAINER;

public class BackpackContainer extends Container {

    public static final ICCLContainerFactory<BackpackContainer> FACTORY = (windowId, playerInv, packet) -> new BackpackContainer(windowId, playerInv);

    private final BackpackInventory inventory = new BackpackInventory(27);

    private final PlayerInventory playerInventory;

    public BackpackContainer(int windowId, PlayerInventory playerInventory) {
        super(BACKPACK_CONTAINER, windowId);

        this.playerInventory = playerInventory;

        InventoryLib.addPlayerInventory(playerInventory, 8, 86, this::addSlot);
        InventoryLib.addInventory(inventory, 0, 8, 18, 9, 3, this::addSlot);

        inventory.addListener(this::onBackpackInventoryChanged);
        inventory.loadInventoryFromMainHand(playerInventory);
    }

    @Override
    public ItemStack clicked(int slotId, int dragType, ClickType clickType, PlayerEntity player) {
        return super.clicked(slotId, dragType, clickType, player);
    }

    @Override
    public void slotsChanged(IInventory inventory) {
        super.slotsChanged(inventory);
        //TODO save bag on server
        // Note: Not reliable, not always called
    }

    @Override
    public void removed(PlayerEntity player) {
        super.removed(player);
        if (!player.level.isClientSide) {
            inventory.saveInventoryToMainHand(playerInventory);
        }
    }

    @Override
    public boolean stillValid(PlayerEntity player) {
        ItemStack stack = player.getMainHandItem();
        return BackpackItem.isBackpack(stack);
    }

    protected void onBackpackInventoryChanged(IInventory backpackInventory) {
        //TODO save bag on server
    }
}
