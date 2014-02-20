package mrtjp.projectred.transportation;

import codechicken.core.IGuiPacketSender;
import codechicken.core.ServerUtils;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;
import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.ItemDataCard;
import mrtjp.projectred.core.inventory.GhostContainer2;
import mrtjp.projectred.core.inventory.GhostContainer2.ISlotController;
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended;
import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.core.utils.PostponedWorkItem;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RequestBranchNode.CraftingPromise;
import mrtjp.projectred.transportation.RequestBranchNode.DeliveryPromise;
import mrtjp.projectred.transportation.RequestBranchNode.ExcessPromise;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;

import java.util.*;
import java.util.concurrent.DelayQueue;

public class RoutedCraftingPipePart extends RoutedJunctionPipePart implements IWorldCrafter
{
    public SimpleInventory chipSlots = new SimpleInventory(8, "chips", 1)
    {
        @Override
        public void onInventoryChanged()
        {
            super.onInventoryChanged();
            refreshChips();
        }

        @Override
        public boolean isItemValidForSlot(int i, ItemStack stack)
        {
            return stack != null
                    && stack.getItem() instanceof ItemRoutingChip
                    && stack.hasTagCompound()
                    && stack.getTagCompound().hasKey("chipROM")
                    && EnumRoutingChip.getForStack(stack).isCraftingChip();
        }
    };

    public ChipCrafting[] chips = new ChipCrafting[9];

    private SimpleInventory cardSlots = new SimpleInventory(9, "links", 1)
    {
        @Override
        public boolean isItemValidForSlot(int i, ItemStack stack)
        {
            if (ItemDataCard.hasCardData(stack))
            {
                NBTTagCompound tag = ItemDataCard.loadData(stack, "ext_pipe");
                if (tag.hasKey("id"))
                    return true;
            }
            return false;
        }

        @Override
        public void onInventoryChanged()
        {
            refreshExtensions();
        }
    };

    private DeliveryManager manager = new DeliveryManager();

    private final LinkedList<Pair2<ItemKeyStack, IWorldRequester>> excess = new LinkedList<Pair2<ItemKeyStack, IWorldRequester>>();
    private final DelayQueue<PostponedWorkItem<ItemKeyStack>> lost = new DelayQueue<PostponedWorkItem<ItemKeyStack>>();

    private final int[] extensionIPs = new int[9];

    public int priority = 0;

    private int remainingDelay = operationDelay();
    private int remainingDelay2 = operationDelay2();

    /**
     * Standard operation delay
     */
    private int operationDelay()
    {
        return 10;
    }

    /**
     * Lost items handling delay
     */
    private int operationDelay2()
    {
        return 40;
    }

    protected int itemsToExtract()
    {
        return 1;
    }

    protected int stacksToExtract()
    {
        return 1;
    }

    @Override
    public void read(MCDataInput packet, int switch_key)
    {
        if (switch_key == 45)
            priority = packet.readInt();
        else
            super.read(packet, switch_key);
    }

    public void priorityUp()
    {
        int old = priority;
        priority = Math.min(16, priority + 1);
        if (old != priority)
            sendPriorityUpdate();
    }

    public void priorityDown()
    {
        int old = priority;
        priority = Math.max(-16, priority - 1);
        if (old != priority)
            sendPriorityUpdate();
    }

    private void sendPriorityUpdate()
    {
        if (!world().isRemote)
            tile().getWriteStream(this).writeByte(45).writeInt(priority);
    }

    @Override
    public void updateServer()
    {
        if (--remainingDelay <= 0)
        {
            remainingDelay = operationDelay();
            operationTick();
        }

        if (--remainingDelay2 <= 0)
        {
            remainingDelay2 = operationDelay2();
            lostHandleTick();
        }
    }

    private void operationTick()
    {
        if (!manager.hasOrders() && excess.isEmpty())
            return;

        IInventory real = getInventory();
        if (real == null)
        {
            if (manager.hasOrders())
                manager.dispatchFailed();
            else
                excess.clear();
            return;
        }
        int side = getInterfacedSide();

        InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);

        List<ItemKeyStack> wanted = getCraftedItems();
        if (wanted == null || wanted.isEmpty())
            return;

        RouteFX.spawnType1(RouteFX.color_checkInv, 8, new BlockCoord(tile()), world());

        int itemsleft = itemsToExtract();
        int stacksleft = stacksToExtract();

        while (itemsleft > 0 && stacksleft > 0 && (manager.hasOrders() || !excess.isEmpty()))
        {
            Pair2<ItemKeyStack, IWorldRequester> nextOrder;
            boolean processingOrder = false;
            if (manager.hasOrders())
            {
                nextOrder = manager.peek();
                processingOrder = true;
            }
            else
                nextOrder = excess.getFirst();

            ItemKeyStack keyStack = nextOrder.getValue1();
            int maxToSend = Math.min(itemsleft, keyStack.stackSize);
            maxToSend = Math.min(keyStack.key().getMaxStackSize(), maxToSend);
            int available = inv.extractItem(keyStack.key(), maxToSend);

            if (available <= 0)
                break;

            ItemKey key = keyStack.key();
            while (available > 0)
            {
                int numToSend = Math.min(available, key.getMaxStackSize());
                numToSend = Math.min(numToSend, keyStack.stackSize);
                if (numToSend == 0)
                    break;
                stacksleft -= 1;
                itemsleft -= numToSend;
                available -= numToSend;
                ItemStack toSend = key.makeStack(numToSend);

                if (processingOrder)
                {
                    queueStackToSend(toSend, side, SendPriority.ACTIVE, nextOrder.getValue2().getRouter().getIPAddress());
                    manager.dispatchSuccessful(numToSend, false);

                    if (manager.hasOrders())
                        nextOrder = manager.peek();
                    else
                    {
                        processingOrder = false;
                        if (!excess.isEmpty())
                            nextOrder = excess.getFirst();
                    }
                }
                else
                {
                    removeExcess(key, numToSend);
                    queueStackToSend(toSend, side, SendPriority.WANDERING, -1);
                }
            }
        }
    }

    private void lostHandleTick()
    {
        if (lost.isEmpty())
            return;

        PostponedWorkItem<ItemKeyStack> post;

        while ((post = lost.poll()) != null)
        {
            ItemKeyStack stack = post.getItem();
            int toRequest = stack.stackSize;
            toRequest = Math.min(toRequest, getActiveFreeSpace(stack.key()));

            if (toRequest <= 0)
            {
                lost.add(new PostponedWorkItem<ItemKeyStack>(stack, 5000));
                continue;
            }

            RequestConsole req = new RequestConsole().setDestination(this);
            req.setPulling(true).setCrafting(true).setPartials(true);

            int requested = req.makeRequest(ItemKeyStack.get(stack.key(), toRequest)).requested();

            if (requested < stack.stackSize)
            {
                stack.stackSize -= requested;
                lost.add(new PostponedWorkItem<ItemKeyStack>(stack, 5000));
            }
        }
    }

    @Override
    public void trackedItemLost(ItemKeyStack s)
    {
        lost.add(new PostponedWorkItem<ItemKeyStack>(s, 5000));
    }

    private void removeExcess(ItemKey item, int amount)
    {
        Iterator<Pair2<ItemKeyStack, IWorldRequester>> iter = excess.iterator();
        while (iter.hasNext())
        {
            ItemKeyStack stack = iter.next().getValue1();
            if (stack.key().equals(item))
                if (amount >= stack.stackSize)
                {
                    amount -= stack.stackSize;
                    iter.remove();
                    if (amount <= 0)
                        return;
                }
                else
                {
                    stack.stackSize -= amount;
                    break;
                }
        }
    }

    private void refreshExtensions()
    {
        for (int i = 0; i < 9; i++)
        {
            ItemStack inslot = cardSlots.getStackInSlot(i);
            if (inslot != null && ItemDataCard.hasCardData(inslot))
            {
                NBTTagCompound data = ItemDataCard.loadData(inslot, "ext_pipe");
                if (data.hasKey("id"))
                {
                    UUID id;
                    try
                    {
                        id = UUID.fromString(data.getString("id"));
                    }
                    catch (Throwable t)
                    {
                        continue;
                    }

                    extensionIPs[i] = RouterServices.instance.getIPforUUID(id);
                }
            }
            else
                extensionIPs[i] = -1;
        }
    }

    public void refreshChips()
    {
        for (int i = 0; i < 8; i++)
        {
            ItemStack stack = chipSlots.getStackInSlot(i);
            RoutingChipset c = ItemRoutingChip.loadChipFromItemStack(stack);
            if (c instanceof ChipCrafting)
            {
                ChipCrafting c2 = (ChipCrafting) c;
                c2.setEnvironment(this, this, i);
                if (chips[i] != c2)
                    chips[i] = c2;
            }
        }
    }

    private IWorldRequester getExtensionFor(int slot)
    {
        if (slot < 0 || slot >= 9)
            return this;

        if (extensionIPs[slot] >= 0 && getRouter().canRouteTo(extensionIPs[slot]))
        {
            Router r = RouterServices.instance.getRouter(extensionIPs[slot]);
            if (r != null && r.isLoaded() && r.getParent() instanceof IWorldRequester)
                return (IWorldRequester) r.getParent();
        }
        return this;
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item)
    {
        if (super.activate(player, hit, item))
            return true;

        if (item != null && item.getItem() instanceof ItemRoutingChip)
        {
            for (int i = 0; i < chipSlots.getSizeInventory(); i++)
                if (chipSlots.getStackInSlot(i) == null && chipSlots.isItemValidForSlot(i, item))
                {
                    ItemStack chip = item.splitStack(1);
                    chipSlots.setInventorySlotContents(i, chip);
                    chipSlots.onInventoryChanged();
                    return true;
                }
        }

        openGui(player);
        return true;
    }

    @Override
    public void onRemoved()
    {
        super.onRemoved();
        if (!world().isRemote)
        {
            chipSlots.dropContents(world(), x(), y(), z());
            cardSlots.dropContents(world(), x(), y(), z());
        }
    }

    public void openGui(EntityPlayer player)
    {
        if (world().isRemote)
            return;

        ServerUtils.openSMPContainer((EntityPlayerMP) player, createContainer(player), new IGuiPacketSender()
        {
            @Override
            public void sendPacket(EntityPlayerMP player, int windowId)
            {
                PacketCustom p = new PacketCustom(TransportationSPH.channel(), NetConstants.gui_CraftingPipe_open);
                p.writeCoord(x(), y(), z());
                p.writeByte(windowId);
                p.writeInt(priority);
                p.sendToPlayer(player);
            }
        });
    }

    public Container createContainer(EntityPlayer player)
    {
        GhostContainer2 ghost = new GhostContainer2(player.inventory);

        int s = 0;
        for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 12, 2, 4, 20, 0))
            ghost.addCustomSlot(new SlotExtended(chipSlots, s++, p.getValue1(), p.getValue2()).setCheck(ISlotController.InventoryRulesController.instance));

        int s2 = 0;
        for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(8, 108, 9, 1, 0, 0))
            ghost.addCustomSlot(new SlotExtended(cardSlots, s2++, p.getValue1(), p.getValue2()).setCheck(ISlotController.InventoryRulesController.instance));

        ghost.addPlayerInventory(8, 138);
        return ghost;
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        chipSlots.save(tag, "c");
        cardSlots.save(tag, "l");
        tag.setInteger("pri", priority);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        chipSlots.load(tag, "c");
        cardSlots.load(tag, "l");
        priority = tag.getInteger("pri");
    }

    @Override
    public void requestPromises(RequestBranchNode request, int existingPromises)
    {
        if (excess.isEmpty())
            return;

        ItemKey requestedItem = request.getRequestedPackage();
        List<ItemKeyStack> providedItem = getCraftedItems();
        for (ItemKeyStack item : providedItem)
            if (item.key() == requestedItem)
                return;

        boolean contains = false;
        for (ItemKeyStack stack : providedItem)
            if (stack.key().equals(requestedItem))
            {
                contains = true;
                break;
            }

        if (!contains)
            return;

        int remaining = 0;
        for (Pair2<ItemKeyStack, IWorldRequester> extra : excess)
            if (extra.getValue1().key() == requestedItem)
                remaining += extra.getValue1().stackSize;

        remaining -= existingPromises;
        if (remaining <= 0)
            return;

        ExcessPromise promise = new ExcessPromise();
        promise.setUsed(true).setPackage(requestedItem).setSize(Math.min(remaining, request.getMissingCount())).setSender(this);

        request.addPromise(promise);
    }

    @Override
    public void deliverPromises(DeliveryPromise promise, IWorldRequester requester)
    {
        if (promise instanceof ExcessPromise)
            removeExcess(promise.thePackage, promise.size);

        manager.addOrder(ItemKeyStack.get(promise.thePackage, promise.size), requester);
    }

    @Override
    public void getBroadcastedItems(Map<ItemKey, Integer> map)
    {
    }

    @Override
    public CraftingPromise requestCraftPromise(ItemKey item)
    {
        List<ItemKeyStack> items = getCraftedItems();
        if (items == null)
            return null;

        ChipCrafting r = getChipFor(item);
        if (r == null)
            return null;
        ItemKeyStack result = ItemKeyStack.get(r.matrix().getStackInSlot(9));

        IWorldRequester[] requesters = new IWorldRequester[9];
        for (int i = 0; i < 9; i++)
            requesters[i] = getExtensionFor(r.extIndex()[i]);

        CraftingPromise promise = new CraftingPromise(result, this, priority);
        for (int i = 0; i < 9; i++)
        {
            ItemKeyStack keystack = ItemKeyStack.get(r.matrix().getStackInSlot(i));
            if (keystack == null || keystack.stackSize <= 0)
                continue;

            promise.addIngredient(keystack, requesters[i]);
        }

        return promise;
    }

    @Override
    public void registerExcess(DeliveryPromise promise)
    {
        ItemKeyStack keystack = ItemKeyStack.get(promise.thePackage, promise.size);
        excess.add(new Pair2<ItemKeyStack, IWorldRequester>(keystack, null));
    }

    @Override
    public List<ItemKeyStack> getCraftedItems()
    {
        List<ItemKeyStack> list = new ArrayList<ItemKeyStack>(9);
        for (ChipCrafting r : chips)
            if (r != null)
            {
                ItemStack stack = r.matrix().getStackInSlot(9);
                if (stack != null)
                    list.add(ItemKeyStack.get(stack));
            }

        return list;
    }

    public ChipCrafting getChipFor(ItemKey key)
    {
        for (ChipCrafting r : chips)
            if (r != null)
            {
                ItemStack stack = r.matrix().getStackInSlot(9);
                if (stack != null && ItemKey.get(stack).equals(key))
                    return r;
            }

        return null;
    }

    @Override
    public int getPriority()
    {
        return priority;
    }

    @Override
    public double getWorkLoad()
    {
        return (manager.getTotalDeliveryCount() + 63.0) / 64.0;
    }

    @Override
    public int itemsToProcess()
    {
        return manager.getTotalDeliveryCount();
    }

    @Override
    public int getActiveFreeSpace(ItemKey item)
    {
        // Dont craft more than one thing at a time.
        if (manager.hasOrders())
        {
            ChipCrafting r = getChipFor(manager.peek().getValue1().key());
            if (r != null)
            {
                for (int i = 0; i < 10; i++)
                {
                    ItemStack s = r.matrix().getStackInSlot(i);
                    if (s != null && ItemKey.get(s).equals(item))
                        return super.getActiveFreeSpace(item);
                }
            }

            return 0;
        }
        else
            return super.getActiveFreeSpace(item);
    }

    @Override
    public String getType()
    {
        return "pr_rcrafting";
    }
}
