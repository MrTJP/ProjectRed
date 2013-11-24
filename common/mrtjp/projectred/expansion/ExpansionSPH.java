package mrtjp.projectred.expansion;

import java.util.Map;
import java.util.Map.Entry;

import mrtjp.projectred.ProjectRedExpansion;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.item.ItemStack;
import net.minecraft.network.NetServerHandler;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IServerPacketHandler;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class ExpansionSPH implements IServerPacketHandler {
    public static final Object channel = ProjectRedExpansion.instance;

    @Override
    public void handlePacket(PacketCustom packet, NetServerHandler nethandler, EntityPlayerMP sender) {
        switch(packet.getType()) {
        case NetConstants.gui_ChipNBTSet:
            setChipNBT(packet, sender);
            break;
        case NetConstants.gui_CraftingPipe_action:
            handleCraftingPipeAction(packet, sender.worldObj);
            break;
        case NetConstants.gui_Request_action:
            handleRequestAction(packet, sender);
            break;
        case NetConstants.gui_Request_submit:
            handleRequestSubmit(packet, sender);
            break;
        case NetConstants.gui_Request_listRefresh:
            handleRequestListRefresh(packet, sender);
            break;
        }
    }

    private void handleRequestListRefresh(PacketCustom packet, EntityPlayerMP sender) {
        BlockCoord bc = packet.readCoord();
        TMultiPart t = BasicUtils.getMultiPart(sender.worldObj, bc, 6);
        if (t instanceof IWorldRequester)
            sendRequestList((IWorldRequester) t, sender, packet.readBoolean(), packet.readBoolean());
    }

    private void handleRequestAction(PacketCustom packet, EntityPlayerMP sender) {
        BlockCoord bc = packet.readCoord();
        TMultiPart t = BasicUtils.getMultiPart(sender.worldObj, bc, 6);
        if (t instanceof IWorldRequester) {
            String ident = packet.readString();
            //ADD THINGS HERE
        }
    }

    private static void sendRequestList(IWorldRequester requester, EntityPlayerMP player, boolean collectBroadcast, boolean collectCrafts) {
        CollectionPathFinder cpf = new CollectionPathFinder().setRequester(requester);
        cpf.setCollectBroadcasts(collectBroadcast).setCollectCrafts(collectCrafts);

        Map<ItemKey, Integer> map = cpf.collect().getCollection();

        PacketCustom packet2 = new PacketCustom(channel, NetConstants.gui_Request_list);

        packet2.writeInt(map.size());
        for (Entry<ItemKey, Integer> entry : map.entrySet())
            packet2.writeItemStack(entry.getKey().makeStack(entry.getValue() == null ? 0 : entry.getValue()), true);
        packet2.compressed().sendToPlayer(player);
    }

    private void handleRequestSubmit(PacketCustom packet, EntityPlayerMP sender) {
        TMultiPart t = BasicUtils.getMultiPart(sender.worldObj, packet.readCoord(), 6);
        if (t instanceof IWorldRequester) {
            RequestConsole r = new RequestConsole().setDestination((IWorldRequester) t);

            boolean pull = packet.readBoolean();
            boolean craft = packet.readBoolean();
            boolean partial = packet.readBoolean();

            r.setCrafting(craft).setPulling(pull).setPartials(partial);

            ItemKeyStack s = ItemKeyStack.get(packet.readItemStack(true));
            r.makeRequest(s);

            if (r.requested() > 0)
                sender.addChatMessage("Successfully requested " + r.requested() + " of " + s.getName() + ".");
            else {
                sender.addChatMessage("Could not request " + s.stackSize + " of " + s.getName() + ". Missing:");
                for (Entry<ItemKey, Integer> entry : r.getMissing().entrySet())
                    sender.addChatMessage(entry.getValue() + " of " + entry.getKey().getName());
            }

            sendRequestList((IWorldRequester)t, sender, pull, craft);
        }
    }

    private void setChipNBT(PacketCustom packet, EntityPlayerMP player) {
        int slot = packet.readByte();
        ItemStack stack = packet.readItemStack();
        player.inventory.setInventorySlotContents(slot, stack);
        player.inventory.onInventoryChanged();
    }

    private void handleCraftingPipeAction(PacketCustom packet, World w) {
        TMultiPart t = BasicUtils.getMultiPart(w, packet.readCoord(), 6);
        if (t instanceof RoutedCraftingPipePart) {
            RoutedCraftingPipePart pipe = (RoutedCraftingPipePart) t;
            String action = packet.readString();
            if (action.equals("up"))
                pipe.priorityUp();
            else if (action.equals("down"))
                pipe.priorityDown();
        }
    }
}
