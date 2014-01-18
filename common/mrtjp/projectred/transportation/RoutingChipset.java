package mrtjp.projectred.transportation;

import codechicken.core.IGuiPacketSender;
import codechicken.core.ServerUtils;
import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RequestBranchNode.DeliveryPromise;
import mrtjp.projectred.transportation.RoutingChipContainerFactory.ChipGhostContainer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public abstract class RoutingChipset
{
    private IInventoryProvider inventoryProvider;
    private IRouteLayer routeLayer;
    private int slot;

    public void setEnvironment(IInventoryProvider inventoryProvider, IRouteLayer routeLayer, int slot)
    {
        this.inventoryProvider = inventoryProvider;
        this.routeLayer = routeLayer;
        this.slot = slot;
    }

    public IInventoryProvider inventoryProvider()
    {
        return inventoryProvider;
    }

    public IRouteLayer routeLayer()
    {
        return routeLayer;
    }

    public int slot()
    {
        return slot;
    }

    public void update()
    {
    }

    /** Syncing **/
    public SyncResponse getSyncResponse(ItemKey item, SyncResponse rival)
    {
        return null;
    }

    /** Broadcasting **/
    public void requestPromises(RequestBranchNode request, int existingPromises)
    {
    }
    public void deliverPromises(DeliveryPromise promise, IWorldRequester requester)
    {
    }
    public void getProvidedItems(Map<ItemKey, Integer> map)
    {
    }
    public int getPriority()
    {
        return Integer.MIN_VALUE;
    }
    public double getWorkLoad()
    {
        return 0;
    }

    /** Requesting **/
    public void trackedItemLost(ItemKeyStack s)
    {
    }
    public void trackedItemReceived(ItemKeyStack s)
    {
    }

    /** World interactions **/
    public void onPipeBroken()
    {
    }
    public void onNeighborTileChanged(int side, boolean weak)
    {
    }
    public boolean weakTileChanges()
    {
        return false;
    }

    public void save(NBTTagCompound tag)
    {
        NBTTagCompound tag2 = new NBTTagCompound();
        getUpgradeBus().save(tag2);
        tag.setTag("upgrd", tag2);
    }

    public void load(NBTTagCompound tag)
    {
        NBTTagCompound tag2 = tag.getCompoundTag("upgrd");
        getUpgradeBus().load(tag2);
    }

    public abstract List<String> infoCollection();

    public abstract EnumRoutingChip getChipType();

    public void openGui(EntityPlayer player)
    {
        if (player.worldObj.isRemote)
            return;

        ServerUtils.openSMPContainer((EntityPlayerMP) player, createContainer(player), new IGuiPacketSender() {
            @Override
            public void sendPacket(EntityPlayerMP player, int windowId)
            {
                PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Chipset_open);
                packet.writeByte(player.inventory.currentItem);
                packet.writeByte(windowId);
                packet.sendToPlayer(player);
            }
        });
    }

    public ChipGhostContainer createContainer(EntityPlayer player)
    {
        ChipGhostContainer<RoutingChipset> ghost = new ChipGhostContainer<RoutingChipset>(player);
        ghost.addPlayerInventory(8, 86);
        return ghost;
    }

    /** Upgrade bus settings **/

    private final UpgradeBus upgradeBus = createUpgradeBus();

    public UpgradeBus createUpgradeBus()
    {
        UpgradeBus bus = new UpgradeBus(0, 0);
        return bus;
    }

    public UpgradeBus getUpgradeBus()
    {
        return upgradeBus;
    }

    public void addUpgradeBusInfo(List<String> list)
    {
        List<String> list2 = new LinkedList<String>();

        UpgradeBus b = getUpgradeBus();
        if (b.containsUpgrades())
        {
            list2.add("--- upgrades ---");

            String s = "";
            if (b.Lset[0])
                s = s + "LX";
            if (b.Lset[1])
                s = s + " - LY";
            if (b.Lset[2])
                s = s + " - LZ";
            if (!s.isEmpty())
                list2.add(s);

            String s2 = "";
            if (b.Rset[0])
                s2 = s2 + "RX";
            if (b.Rset[1])
                s2 = s2 + " - RY";
            if (b.Rset[2])
                s2 = s2 + " - RZ";
            if (!s2.isEmpty())
                list2.add(s2);

            list2.add("----------------");

            for (int i = 0; i < list2.size(); i++)
                list2.set(i, EnumChatFormatting.GRAY + list2.get(i));

            list.addAll(list2);
        }
    }

    public static class UpgradeBus
    {
        public boolean[] Lset = new boolean[3];

        public boolean[] Rset = new boolean[3];

        public int LXLatency = 0;
        public int LYLatency = 0;
        public int LZLatency = 0;

        public int RXLatency = 0;
        public int RYLatency = 0;
        public int RZLatency = 0;

        public final int maxL;
        public final int maxR;

        public String Linfo;
        public String Lformula;

        public String Rinfo;
        public String Rformula;

        public UpgradeBus(int maxL, int maxR)
        {
            this.maxL = maxL;
            this.maxR = maxR;
        }

        public int LLatency()
        {
            int count = 0;
            if (Lset[0])
                count += LXLatency;
            if (Lset[1])
                count += LYLatency;
            if (Lset[2])
                count += LZLatency;
            return count;
        }

        public int RLatency()
        {
            int count = 0;
            if (Rset[0])
                count += RXLatency;
            if (Rset[1])
                count += RYLatency;
            if (Rset[2])
                count += RZLatency;
            return count;
        }

        public UpgradeBus setLatency(int lx, int ly, int lz, int rx, int ry, int rz)
        {
            LXLatency = lx;
            LYLatency = ly;
            LZLatency = lz;

            RXLatency = rx;
            RYLatency = ry;
            RZLatency = rz;
            return this;
        }

        public boolean installL(int i, boolean doInstall)
        {
            if (i >= maxL)
                return false;

            if (i-1 >= 0 && !Lset[i-1])
                return false;

            if (!Lset[i])
            {
                if (doInstall)
                    Lset[i] = true;
                return true;
            }
            return false;
        }

        public boolean installR(int i, boolean doInstall)
        {
            if (i >= maxR)
                return false;

            if (i-1 >= 0 && !Rset[i-1])
                return false;

            if (!Rset[i])
            {
                if (doInstall)
                    Rset[i] = true;
                return true;
            }
            return false;
        }

        public boolean containsUpgrades()
        {
            for (int i = 0; i < 3; i++)
            {
                if (Lset[i] || Rset[i]) return true;
            }
            return false;
        }

        public void save(NBTTagCompound tag)
        {
            for (int i = 0; i < 3; i++)
            {
                tag.setBoolean("L"+i, Lset[i]);
                tag.setBoolean("R"+i, Rset[i]);
            }
        }

        public void load(NBTTagCompound tag)
        {
            for (int i = 0; i < 3; i++)
            {
                Lset[i] = tag.getBoolean("L"+i);
                Rset[i] = tag.getBoolean("R"+i);
            }
        }
    }
}
