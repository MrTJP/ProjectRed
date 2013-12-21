package mrtjp.projectred.transportation;

import mrtjp.projectred.ProjectRedTransportation;
import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.ItemPart;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.inventory.GhostContainer2;
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.transportation.RoutingChipset.UpgradeBus;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import codechicken.core.IGuiPacketSender;
import codechicken.core.ServerUtils;
import codechicken.lib.packet.PacketCustom;

public class ItemRouterUtility extends Item
{
    public ItemRouterUtility(int par1)
    {
        super(par1);
        setUnlocalizedName("projectred.transportation.routerutil");
        setCreativeTab(ProjectRedTransportation.tabTransportation);
    }

    @Override
    public ItemStack onItemRightClick(ItemStack stack, World w, EntityPlayer player)
    {
        if (!w.isRemote && stack != null && stack.getItem() instanceof ItemRouterUtility)
        {
            openGui(player);
        }
        return super.onItemRightClick(stack, w, player);
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int par4, int par5, int par6, int par7, float par8, float par9, float par10)
    {
        if (!w.isRemote && stack != null && stack.getItem() instanceof ItemRouterUtility)
        {
            openGui(player);
        }
        return true;
    }
    
    @Override
    public void registerIcons(IconRegister reg)
    {
        this.itemIcon = reg.registerIcon("projectred:routerutil");
    }
    
    private void openGui(EntityPlayer player)
    {
        if (player.worldObj.isRemote)
            return;

        ServerUtils.openSMPContainer((EntityPlayerMP) player, new ChipUpgradeContainer(player), new IGuiPacketSender() {
            @Override
            public void sendPacket(EntityPlayerMP player, int windowId)
            {
                PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_RouterUtil_open);
                packet.writeByte(windowId);
                packet.sendToPlayer(player);
            }
        });
    }
        
    public static class ChipUpgradeContainer extends GhostContainer2
    {
        public SimpleInventory upgradeInv;
        
        private RoutingChipset chip;
        
        private final int slot;
        
        public ChipUpgradeContainer(EntityPlayer player)
        {
            super(player.inventory);
            
            slot = player.inventory.currentItem;
            addPlayerInventory(8, 86);

            upgradeInv = new SimpleInventory(7, "upBus", 1) {
                @Override
                public boolean isItemValidForSlot(int slot, ItemStack stack)
                {
                    if (slot == 6)
                    {
                        return stack != null && stack.getItem() instanceof ItemRoutingChip && stack.hasTagCompound() && stack.getTagCompound().hasKey("chipROM");
                    }
                    if (stack.getItem() instanceof ItemPart)
                    {
                        int slotForMeta = stack.getItemDamage() - EnumPart.CHIPUPGRADE_LX.meta;
                        return slotForMeta == slot;
                    }
                    return false;
                }
                
                @Override
                public void onInventoryChanged()
                {
                    refreshChips();
                }
            };
            
            int s = 0;
            for (Pair2<Integer, Integer> coord : BasicGuiUtils.createSlotArray(8, 18, 1, 3, 2, 2))
            {
                SlotExtended slot = new SlotExtended(upgradeInv, s++, coord.getValue1(), coord.getValue2());
                addCustomSlot(slot);
            }
            for (Pair2<Integer, Integer> coord : BasicGuiUtils.createSlotArray(152, 18, 1, 3, 2, 2))
            {
                SlotExtended slot = new SlotExtended(upgradeInv, s++, coord.getValue1(), coord.getValue2());
                addCustomSlot(slot);
            }
            addCustomSlot(new SlotExtended(upgradeInv, 6, 80, 38));
        }
        
        @Override
        public void onContainerClosed(EntityPlayer p)
        {
            super.onContainerClosed(p);

            for (int i = 0; i < upgradeInv.getSizeInventory(); i++)
                while (upgradeInv.getStackInSlot(i) != null)
                {
                    ItemStack todrop = upgradeInv.decrStackSize(i, upgradeInv.getStackInSlot(i).getMaxStackSize());
                    p.dropPlayerItem(todrop);
                }
            
            upgradeInv.onInventoryChanged();
        }
        
        @Override
        public Slot addSlotToContainer(Slot s)
        {
            if (s.getSlotIndex() == slot && s.inventory == playerInv)
                if (s instanceof SlotExtended)
                    return super.addSlotToContainer(((SlotExtended) s).setRemoval(false));
            
            return super.addSlotToContainer(s);
        }
        
        private void refreshChips()
        {
            ItemStack stack = getChipStack();
            RoutingChipset c = ItemRoutingChip.loadChipFromItemStack(stack);
            if (chip != c)
                chip = c;
        }
        
        public void installPossibleUpgrades()
        {
            RoutingChipset r = getChip();
            if (r != null)
            {
                UpgradeBus bus = r.getUpgradeBus();
                for (int i = 0; i < 6; i++)
                {
                    ItemStack stack = upgradeInv.getStackInSlot(i);
                    if (stack != null)
                    {
                        if (i < 3)
                        {
                            if (bus.installL(i, true))
                               upgradeInv.setInventorySlotContents(i, null);
                        }
                        else
                        {
                            if (bus.installR(i-3, true))
                                upgradeInv.setInventorySlotContents(i, null);
                        }
                    }
                }
            }
            
            ItemStack chipStack = getChipStack();
            ItemRoutingChip.saveChipToItemStack(chipStack, r);
            upgradeInv.setInventorySlotContents(6, chipStack);
            detectAndSendChanges();
        }
        
        public RoutingChipset getChip()
        {
            return chip;
        }
        
        public ItemStack getChipStack()
        {
            return upgradeInv.getStackInSlot(6);
        }
    }
}
