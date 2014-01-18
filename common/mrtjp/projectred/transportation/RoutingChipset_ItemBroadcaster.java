package mrtjp.projectred.transportation;

import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RequestBranchNode.DeliveryPromise;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;
import org.lwjgl.input.Keyboard;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class RoutingChipset_ItemBroadcaster extends RoutingChipset
{
    public SimpleInventory filter = new SimpleInventory(9, "filter", 1);
    public int extractOrient = -1;
    public boolean filterExclude = true;

    public int preference = 0;

    // 0-none, 1-type, 2-slot
    int hideMode = 0;

    int timeRemaining = operationDelay();

    private final DeliveryManager manager = new DeliveryManager();

    public void prefUp()
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            preference += 10;
        else
            preference += 1;
        if (preference > 100)
            preference = 100;
    }

    public void prefDown()
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            preference -= 10;
        else
            preference -= 1;
        if (preference < -100)
            preference = -100;
    }

    public void shiftOrient()
    {
        extractOrient++;
        if (extractOrient > 5)
            extractOrient = -1;
    }

    public void shiftHiding()
    {
        hideMode = (hideMode + 1) % 3;
    }

    private int stacksToExtract()
    {
        return 1 + getUpgradeBus().LLatency();
    }

    private int itemsToExtract()
    {
        return 8 + getUpgradeBus().RLatency();
    }

    private int operationDelay()
    {
        return 5;
    }

    @Override
    public void update()
    {
        if (--timeRemaining > 0)
            return;
        timeRemaining = operationDelay();

        if (!manager.hasOrders())
            return;

        Pair2<ItemKeyStack, IWorldRequester> next;
        int stacksRemaining = stacksToExtract();
        int itemsRemaining = itemsToExtract();
        while (manager.hasOrders() && (next = manager.peek()) != null && stacksRemaining > 0 && itemsRemaining > 0)
        {
            IInventory real = inventoryProvider().getInventory();
            if (real == null)
            {
                manager.dispatchFailed();
                continue;
            }

            ItemKeyStack reqKeyStack = next.getValue1();
            IWorldRequester requester = next.getValue2();

            int side = extractOrient == -1 ? inventoryProvider().getInterfacedSide() : extractOrient;
            InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);

            if (hideMode == 1)
                inv.setHidePerType(true);
            else if (hideMode == 2)
                inv.setHidePerSlot(true);

            if (!routeLayer().getRouter().canRouteTo(requester.getRouter().getIPAddress()))
            {
                manager.dispatchFailed();
                continue;
            }

            int toExtract = Math.min(inv.getItemCount(reqKeyStack.key()), reqKeyStack.stackSize);
            toExtract = Math.min(toExtract, itemsRemaining);
            toExtract = Math.min(toExtract, reqKeyStack.makeStack().getMaxStackSize());

            boolean reStock = false;
            int destinationSpace = requester.getActiveFreeSpace(reqKeyStack.key());
            if (destinationSpace < toExtract)
            {
                toExtract = destinationSpace;
                if (toExtract <= 0)
                {
                    manager.reStock();
                    break;
                }
                reStock = true;
            }

            int removed = inv.extractItem(reqKeyStack.key(), toExtract);

            if (removed <= 0)
            {
                manager.dispatchFailed();
                continue;
            }

            ItemStack toSend = reqKeyStack.key().makeStack(removed);

            routeLayer().queueStackToSend(toSend, inventoryProvider().getInterfacedSide(), SendPriority.ACTIVE, requester.getRouter().getIPAddress());

            manager.dispatchSuccessful(removed, reStock);

            stacksRemaining -= 1;
            itemsRemaining -= removed;
        }
    }

    @Override
    public void requestPromises(RequestBranchNode request, int existingPromises)
    {
        IInventory real = inventoryProvider().getInventory();
        if (real == null)
            return;
        int side = extractOrient == -1 ? inventoryProvider().getInterfacedSide() : extractOrient;

        InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);
        InventoryWrapper filt = InventoryWrapper.wrapInventory(filter).setSlotsAll();

        ItemKey requested = request.getRequestedPackage();

        if (filt.hasItem(requested) != filterExclude)
        {

            if (hideMode == 1)
                inv.setHidePerType(true);
            else if (hideMode == 2)
                inv.setHidePerSlot(true);

            int numberAvailable = inv.getItemCount(requested);
            numberAvailable -= existingPromises;

            if (numberAvailable > 0)
            {
                DeliveryPromise promise = new DeliveryPromise();
                promise.setPackage(requested).setSize(Math.min(request.getMissingCount(), numberAvailable)).setSender(routeLayer().getBroadcaster());

                request.addPromise(promise);
            }
        }
    }

    @Override
    public void deliverPromises(DeliveryPromise promise, IWorldRequester requester)
    {
        manager.addOrder(ItemKeyStack.get(promise.thePackage, promise.size), requester);
    }

    @Override
    public void getProvidedItems(Map<ItemKey, Integer> map)
    {
        IInventory real = inventoryProvider().getInventory();
        if (real == null)
            return;

        int side = extractOrient == -1 ? inventoryProvider().getInterfacedSide() : extractOrient;

        InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);
        InventoryWrapper filt = InventoryWrapper.wrapInventory(filter).setSlotsAll();
        if (hideMode == 1)
            inv.setHidePerType(true);
        else if (hideMode == 2)
            inv.setHidePerSlot(true);

        Map<ItemKey, Integer> items = inv.getAllItemStacks();
        for (Entry<ItemKey, Integer> entry : items.entrySet())
            if (filt.hasItem(entry.getKey()) != filterExclude)
            {
                Integer current = map.get(entry.getKey());
                int toAdd = entry.getValue() - manager.getDeliveryCount(entry.getKey());

                if (toAdd > 0)
                    if (current == null)
                        map.put(entry.getKey(), toAdd);
                    else
                        map.put(entry.getKey(), current + toAdd);
            }
    }

    @Override
    public int getPriority()
    {
        return preference;
    }

    @Override
    public void onPipeBroken()
    {
        while (manager.hasOrders())
            manager.dispatchFailed();
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        filter.save(tag);
        tag.setBoolean("mode", filterExclude);
        tag.setInteger("pref", preference);
        tag.setInteger("orient", extractOrient);
        tag.setByte("hide", (byte) hideMode);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        filter.load(tag);
        filterExclude = tag.getBoolean("mode");
        preference = tag.getInteger("pref");
        extractOrient = tag.getInteger("orient");
        hideMode = tag.getByte("hide");
    }

    public static final String[] dirs = new String[] { "Down", "Up", "North", "South", "West", "East" };
    public static final String[] hide = new String[] { "off", "one per type", "one per stack" };

    @Override
    public List<String> infoCollection()
    {
        List<String> list = new LinkedList<String>();
        addPriorityInfo(list);
        addOrientInfo(list);
        addFilterInfo(list);
        return list;
    }

    public void addFilterInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Hide Mode: " + hide[hideMode]);
        list.add(EnumChatFormatting.GRAY + "Filter Mode: " + (filterExclude ? "blacklist" : "whitelist"));
        list.add(EnumChatFormatting.GRAY + "Filter: ");
        boolean added = false;
        for (int i = 0; i < filter.getSizeInventory(); i++)
        {
            ItemStack stack = filter.getStackInSlot(i);
            if (stack != null)
            {
                list.add(EnumChatFormatting.GRAY + " - " + stack.getDisplayName());
                added = true;
            }
        }
        if (!added)
            list.add(EnumChatFormatting.GRAY + " - empty");
    }

    public void addPriorityInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Preference: " + preference);
    }

    public void addOrientInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Extract Orientation: " + (extractOrient == -1 ? "Default" : dirs[extractOrient]));
    }

    @Override
    public EnumRoutingChip getChipType()
    {
        return EnumRoutingChip.ITEMBROADCASTER;
    }

    @Override
    public UpgradeBus createUpgradeBus()
    {
        UpgradeBus b = new UpgradeBus(3, 3);
        b.setLatency(1, 2, 4, 8, 16, 32);

        b.Linfo = "stacks to check in one operation";
        b.Lformula = "stacks = 1 + Latency";

        b.Rinfo = "items to extract in one operation";
        b.Rformula = "items = 8 + Latency";
        return b;
    }
}
