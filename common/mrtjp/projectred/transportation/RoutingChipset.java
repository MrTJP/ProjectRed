package mrtjp.projectred.transportation;

import java.util.List;
import java.util.Map;

import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RequestBranchNode.DeliveryPromise;
import mrtjp.projectred.transportation.RoutingChipContainerFactory.ChipGhostContainer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.inventory.Container;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.core.IGuiPacketSender;
import codechicken.core.ServerUtils;
import codechicken.lib.packet.PacketCustom;

public abstract class RoutingChipset {

    private IInventoryProvider inventoryProvider;
    private IRouteLayer routeLayer;
    private int slot;

    public void setEnvironment(IInventoryProvider inventoryProvider, IRouteLayer routeLayer, int slot) {
        this.inventoryProvider = inventoryProvider;
        this.routeLayer = routeLayer;
        this.slot = slot;
    }

    public IInventoryProvider inventoryProvider() {
        return inventoryProvider;
    }

    public IRouteLayer routeLayer() {
        return routeLayer;
    }

    public int slot() {
        return slot;
    }

    public void update() {
    }

    /** Syncing **/
    public SyncResponse getSyncResponse(ItemKey item, SyncResponse rival) {
        return null;
    }

    /** Broadcasting **/
    public void requestPromises(RequestBranchNode request, int existingPromises) {
    }
    public void deliverPromises(DeliveryPromise promise, IWorldRequester requester) {
    }
    public void getProvidedItems(Map<ItemKey, Integer> map) {
    }
    public int getPriority() {
        return Integer.MIN_VALUE;
    }
    public double getWorkLoad() {
        return 0;
    }

    /** Requesting **/
    public void trackedItemLost(ItemKeyStack s) {
    }
    public void trackedItemReceived(ItemKeyStack s) {
    }

    /** World interactions **/
    public void onPipeBroken(){
    }
    public void onNeighborTileChanged(int side, boolean weak){
    }
    public boolean weakTileChanges() {
        return false;
    }

    public abstract void save(NBTTagCompound tag);

    public abstract void load(NBTTagCompound tag);

    public abstract List<String> infoCollection();
    
    public abstract EnumRoutingChip getChipType();

    public void openGui(EntityPlayer player) {
        if (player.worldObj.isRemote)
            return;

        ServerUtils.openSMPContainer((EntityPlayerMP) player, createContainer(player), new IGuiPacketSender() {
            @Override
            public void sendPacket(EntityPlayerMP player, int windowId) {
                PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Chipset_open);
                packet.writeByte(player.inventory.currentItem);
                packet.writeByte(windowId);
                packet.sendToPlayer(player);
            }
        });
    }

    public Container createContainer(EntityPlayer player) {
        ChipGhostContainer<RoutingChipset> ghost = new ChipGhostContainer<RoutingChipset>(player);
        ghost.addPlayerInventory(8, 86);
        return ghost;
    }
}
