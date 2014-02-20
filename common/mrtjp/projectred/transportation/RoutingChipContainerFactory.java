package mrtjp.projectred.transportation;

import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.core.inventory.GhostContainer2;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;

public class RoutingChipContainerFactory
{
    public static class ChipContainer<T extends RoutingChipset> extends GhostContainer2
    {
        EntityPlayer player;
        ItemStack stack;
        int slot;
        T chip;

        public ChipContainer(EntityPlayer player)
        {
            this(player, null);
        }

        public ChipContainer(EntityPlayer player, T chip)
        {
            super(player.inventory);
            this.player = player;
            this.slot = player.inventory.currentItem;
            this.stack = player.inventory.mainInventory[slot];
            this.chip = chip != null ? chip : (T) ItemRoutingChip.loadChipFromItemStack(stack);
        }

        public ChipContainer<T> getNewInstance()
        {
            ChipContainer<T> c = new ChipContainer<T>(player, chip);
            return c;
        }

        public T getChip()
        {
            return chip;
        }

        @Override
        public void onContainerClosed(EntityPlayer player)
        {
            super.onContainerClosed(player);
            if (player.worldObj.isRemote)
            {
                ItemRoutingChip.saveChipToItemStack(player.inventory.mainInventory[slot], chip);
                player.inventory.onInventoryChanged();

                new PacketCustom(TransportationCPH.channel(), NetConstants.gui_ChipNBTSet).writeByte(slot).writeItemStack(player.inventory.mainInventory[slot]).sendToServer();
            }
        }

        @Override
        public Slot addSlotToContainer(Slot slot)
        {
            if (slot.getSlotIndex() == this.slot)
                if (slot instanceof SlotExtended)
                    return super.addSlotToContainer(((SlotExtended) slot).setRemoval(false));

            return super.addSlotToContainer(slot);
        }
    }
}
