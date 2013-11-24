package mrtjp.projectred.expansion;

import mrtjp.projectred.core.inventory.GhostContainer2;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;
import codechicken.lib.packet.PacketCustom;

public class RoutingChipset_ContainerFactory {

    public static class ChipGhostContainer<T extends RoutingChipset> extends GhostContainer2 {
        EntityPlayer player;
        ItemStack stack;
        int slot;
        T chip;

        public ChipGhostContainer(EntityPlayer player) {
            this(player, null);
        }

        public ChipGhostContainer(EntityPlayer player, T chip) {
            super(player.inventory);
            this.player = player;
            this.slot = player.inventory.currentItem;
            this.stack = player.inventory.mainInventory[slot];
            this.chip = chip != null ? chip : (T) ItemRoutingChip.loadChipFromItemStack(stack);
        }

        public ChipGhostContainer<T> getNewInstance() {
            return new ChipGhostContainer<T>(player, chip);
        }

        public T getChip() {
            return chip;
        }

        @Override
        public void onContainerClosed(EntityPlayer player) {
            super.onContainerClosed(player);
            if (player.worldObj.isRemote) {
                ItemRoutingChip.saveChipToItemStack(player.inventory.mainInventory[slot], chip);
                player.inventory.onInventoryChanged();

                new PacketCustom(ExpansionCPH.channel, NetConstants.gui_ChipNBTSet)
                .writeByte(slot).writeItemStack(player.inventory.mainInventory[slot])
                .sendToServer();
            }
        }

        @Override
        public Slot addSlotToContainer(Slot slot) {
            if (slot.getSlotIndex() == this.slot)
                if (slot instanceof SlotExtended)
                    return super.addSlotToContainer(((SlotExtended) slot).setRemoval(false));

            return super.addSlotToContainer(slot);
        }
    }

}
