package mrtjp.projectred.transportation;

import codechicken.core.IGuiPacketSender;
import codechicken.core.ServerUtils;
import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.core.ItemDataCard;
import mrtjp.projectred.core.inventory.GhostContainer2;
import mrtjp.projectred.core.inventory.GhostContainer2.ISlotController;
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.ItemKeyStack;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.inventory.Container;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;

import java.util.LinkedList;

public class RoutedExtensionPipePart extends RoutedJunctionPipePart
{
    public SimpleInventory cardslot = new SimpleInventory(2, "card", 1) {

        @Override
        public boolean isItemValidForSlot(int i, ItemStack stack) {
            return stack != null && stack.getItem() instanceof ItemDataCard;
        }
    };

    private final LinkedList<ItemKeyStack> lost = new LinkedList<ItemKeyStack>();

    private int remainingDelay = operationDelay();

    /**
     * Lost items handling delay
     */
    private int operationDelay()
    {
        return 40;
    }

    @Override
    protected void updateServer()
    {
        if (--remainingDelay <= 0)
        {
            remainingDelay = operationDelay();
            lostHandleTick();
        }

        handleCardsTick();
    }

    private void handleCardsTick()
    {
        if (cardslot.getStackInSlot(0) != null && cardslot.getStackInSlot(1) == null)
        {
            ItemStack stack = cardslot.getStackInSlot(0);

            if (stack.getItem() instanceof ItemDataCard)
            {
                NBTTagCompound data = new NBTTagCompound();
                String id = getRouter().getID().toString();
                data.setString("id", id);
                String s = "- ID: " + id.split("-")[0] + "...";

                ItemDataCard.nameData(data, "Extension pipe data", s);
                ItemDataCard.saveData(stack, data, "ext_pipe");
            }

            cardslot.setInventorySlotContents(0, null);
            cardslot.setInventorySlotContents(1, stack);
            cardslot.onInventoryChanged();
        }
    }

    private void lostHandleTick()
    {
        if (lost.isEmpty())
            return;

        ItemKeyStack stack;

        while ((stack = lost.poll()) != null)
        {
            int toRequest = stack.stackSize;
            toRequest = Math.min(toRequest, getActiveFreeSpace(stack.key()));

            if (toRequest <= 0)
            {
                lost.add(stack);
                continue;
            }

            RequestConsole req = new RequestConsole().setDestination(this);
            req.setPulling(true).setCrafting(true).setPartials(true);

            int requested = req.makeRequest(ItemKeyStack.get(stack.key(), toRequest)).requested();

            if (requested < stack.stackSize)
            {
                stack.stackSize -= requested;
                lost.add(stack);
            }
        }
    }

    @Override
    public void trackedItemLost(ItemKeyStack s)
    {
        lost.add(s);
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        cardslot.save(tag);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        cardslot.load(tag);
    }

    @Override
    public void onRemoved()
    {
        if (!world().isRemote)
            cardslot.dropContents(world(), x(), y(), z());
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item)
    {
        if (super.activate(player, hit, item))
            return true;

        openGui(player);

        return true;
    }

    private void openGui(EntityPlayer player)
    {
        if (world().isRemote)
            return;

        ServerUtils.openSMPContainer((EntityPlayerMP) player, createContainer(player), new IGuiPacketSender() {
            @Override
            public void sendPacket(EntityPlayerMP player, int windowId)
            {
                PacketCustom p = new PacketCustom(TransportationSPH.channel(), NetConstants.gui_ExtensionPipe_open);
                p.writeCoord(x(), y(), z());
                p.writeByte(windowId);
                p.writeString(getRouter().getID().toString());
                p.sendToPlayer(player);
            }
        });
    }

    public Container createContainer(EntityPlayer player)
    {
        GhostContainer2 ghost = new GhostContainer2(player.inventory);

        ISlotController sc = ISlotController.InventoryRulesController.instance;

        ghost.addCustomSlot(new SlotExtended(cardslot, 0, 134, 20).setCheck(sc));
        ghost.addCustomSlot(new SlotExtended(cardslot, 1, 134, 50).setPlacement(false));

        ghost.addPlayerInventory(8, 84);
        return ghost;
    }

    @Override
    public String getType()
    {
        return "pr_rextension";
    }
}
