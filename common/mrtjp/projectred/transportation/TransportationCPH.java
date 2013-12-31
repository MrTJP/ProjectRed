package mrtjp.projectred.transportation;

import java.util.HashMap;
import java.util.Map;

import mrtjp.projectred.ProjectRedTransportation;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.transportation.ItemRouterUtility.ChipUpgradeContainer;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import codechicken.core.ClientUtils;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class TransportationCPH implements IClientPacketHandler
{
    public static final Object channel = ProjectRedTransportation.instance;

    @Override
    public void handlePacket(PacketCustom packet, NetClientHandler net, Minecraft mc)
    {
        switch (packet.getType()) {
        case NetConstants.gui_InterfacePipe_open:
            openRoutedInterfacePipeGui(packet, mc.thePlayer);
            break;
        case NetConstants.gui_CraftingPipe_open:
            openCraftingPipeGui(packet, mc.thePlayer);
            break;
        case NetConstants.gui_Chipset_open:
            openChipsetGui(packet, mc.thePlayer);
            break;
        case NetConstants.gui_Request_open:
            openRequestGui(packet, mc);
            break;
        case NetConstants.gui_Request_list:
            receiveRequestList(packet, mc);
            break;
        case NetConstants.particle_Spawn:
            RouteFX.handleClientPacket(packet, mc.theWorld);
            break;
        case NetConstants.gui_RouterUtil_open:
            openRouterUtilGui(packet, mc);
            break;
        case NetConstants.gui_ExtensionPipe_open:
            openExtensionPipeGui(packet, mc);
            break;
        }
    }

    private void openExtensionPipeGui(PacketCustom packet, Minecraft mc)
    {
        TMultiPart p = BasicUtils.getMultiPart(mc.theWorld, packet.readCoord(), 6);
        
        if (p instanceof RoutedExtensionPipePart)
        {
            RoutedExtensionPipePart pipe = (RoutedExtensionPipePart) p;
            ClientUtils.openSMPGui(packet.readUByte(), new GuiExtensionPipe(pipe.createContainer(mc.thePlayer), packet.readString()));
        }
    }
    
    private void openRouterUtilGui(PacketCustom packet, Minecraft mc)
    {
        ClientUtils.openSMPGui(packet.readByte(), new GuiChipUpgrade(new ChipUpgradeContainer(mc.thePlayer)));
    }
    
    private void receiveRequestList(PacketCustom packet, Minecraft mc)
    {
        if (mc.currentScreen instanceof GuiRequester)
        {
            GuiRequester gui = (GuiRequester) mc.currentScreen;
            int size = packet.readInt();
            Map<ItemKey, Integer> map = new HashMap<ItemKey, Integer>(size);
            for (int i = 0; i < size; i++)
            {
                ItemStack stack = packet.readItemStack(true);
                map.put(ItemKey.get(stack), stack.stackSize);
            }
            gui.receiveContentList(map);
        }

    }

    private void openRequestGui(PacketCustom packet, Minecraft mc)
    {
        TMultiPart p = BasicUtils.getMultiPart(mc.thePlayer.worldObj, packet.readCoord(), 6);
        if (p instanceof IWorldRequester)
            mc.displayGuiScreen(new GuiRequester((IWorldRequester) p));
    }

    private void openRoutedInterfacePipeGui(PacketCustom packet, EntityPlayer player)
    {
        TMultiPart p = BasicUtils.getMultiPart(player.worldObj, packet.readCoord(), 6);
        if (p instanceof RoutedInterfacePipePart)
        {
            RoutedInterfacePipePart pipe = (RoutedInterfacePipePart) p;
            ClientUtils.openSMPGui(packet.readUByte(), new GuiInterfacePipe(pipe.createContainer(player), pipe));
        }
    }

    private void openCraftingPipeGui(PacketCustom packet, EntityPlayer player)
    {
        TMultiPart p = BasicUtils.getMultiPart(player.worldObj, packet.readCoord(), 6);
        if (p instanceof RoutedCraftingPipePart)
        {
            RoutedCraftingPipePart pipe = (RoutedCraftingPipePart) p;
            ClientUtils.openSMPGui(packet.readUByte(), new GuiCraftingPipe(pipe.createContainer(player), pipe));
            pipe.priority = packet.readInt();
        }
    }

    private void openChipsetGui(PacketCustom packet, EntityPlayer player)
    {
        int slot = packet.readByte();
        player.inventory.currentItem = slot;
        ItemStack stack = player.inventory.getStackInSlot(slot);
        if (stack != null)
        {
            EnumRoutingChip e = EnumRoutingChip.get(stack.getItemDamage());
            if (e != null)
            {
                RoutingChipset r = e.createChipset();
                ClientUtils.openSMPGui(packet.readByte(), RoutingChipGuiFactory.getGui(r.createContainer(player), stack.getItemDamage()));
            }
        }
    }
}
