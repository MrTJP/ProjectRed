package mrtjp.projectred.transportation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;

public class RoutingChipset_ItemStockKeeper extends RoutingChipset
{
    public SimpleInventory filter = new SimpleInventory(9, "filter", 127);

    public boolean requestWhenEmpty = false;

    private final HashMap<ItemKey, Integer> enrouteItems = new HashMap<ItemKey, Integer>();

    private int remainingDelay = operationDelay();

    private int operationDelay()
    {
        return 100;
    }

    private int operationsWithoutRequest = 0;

    private int throttleDelay()
    {
        int throttle = 10 * operationsWithoutRequest;
        throttle = Math.min(throttle, 20 * 60);

        return throttle;
    }

    @Override
    public void update()
    {
        if (--remainingDelay > 0)
            return;

        IInventory real = inventoryProvider().getInventory();
        int side = inventoryProvider().getInterfacedSide();
        if (real == null || side < 0)
            return;

        InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);
        InventoryWrapper filt = InventoryWrapper.wrapInventory(filter).setSlotsAll();

        List<ItemKey> checked = new ArrayList<ItemKey>(9);

        boolean requestAttempted = false;
        boolean requestedSomething = false;

        for (int i = 0; i < filter.getSizeInventory(); i++)
        {
            ItemKeyStack keyStack = ItemKeyStack.get(filter.getStackInSlot(i));

            if (keyStack == null || checked.contains(keyStack.key()))
                continue;

            int toRequest = filt.getItemCount(keyStack.key());
            int inInventory = inv.getItemCount(keyStack.key()) + getEnroute(keyStack.key());
            int missing = toRequest - inInventory;

            if (missing <= 0 || (requestWhenEmpty && inInventory > 0))
                continue;

            RequestConsole req = new RequestConsole().setDestination(routeLayer().getRequester());
            req.setCrafting(true).setPulling(true).setPartials(true);
            ItemKeyStack request = ItemKeyStack.get(keyStack.key(), missing);
            req.makeRequest(request);

            requestAttempted = true;

            if (req.requested() > 0)
            {
                addToRequestList(request.key(), req.requested());
                requestedSomething = true;
            }
        }
        if (requestAttempted)
            RouteFX.sendSpawnPacket(RouteFX.color_request, 8, routeLayer().getCoords(), routeLayer().getWorld());

        if (requestAttempted && requestedSomething)
            operationsWithoutRequest = 0;
        else
            operationsWithoutRequest++;

        remainingDelay = operationDelay() + throttleDelay();
    }

    private void addToRequestList(ItemKey item, int amount)
    {
        Integer current = enrouteItems.get(item);
        if (current == null)
            enrouteItems.put(item, amount);
        else
            enrouteItems.put(item, current + amount);
    }

    private void removeFromRequestList(ItemKey item, int amount)
    {
        Integer current = enrouteItems.get(item);
        if (current != null)
        {
            current -= amount;

            if (current <= 0)
                enrouteItems.remove(item);
            else
                enrouteItems.put(item, current);
        }
    }

    private int getEnroute(ItemKey item)
    {
        Integer current = enrouteItems.get(item);
        if (current != null)
            return current.intValue();
        return 0;
    }

    @Override
    public void trackedItemLost(ItemKeyStack s)
    {
        removeFromRequestList(s.key(), s.stackSize);
    }

    @Override
    public void trackedItemReceived(ItemKeyStack s)
    {
        removeFromRequestList(s.key(), s.stackSize);
    }

    @Override
    public boolean weakTileChanges()
    {
        return true;
    }

    @Override
    public void onNeighborTileChanged(int side, boolean weak)
    {
        operationsWithoutRequest = 0;
        remainingDelay = Math.min(remainingDelay, operationDelay());
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        filter.save(tag);
        tag.setBoolean("mode", requestWhenEmpty);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        filter.load(tag);
        requestWhenEmpty = tag.getBoolean("mode");
    }

    @Override
    public List<String> infoCollection()
    {
        List<String> list = new LinkedList<String>();
        addModeInfo(list);
        addFilterInfo(list);
        return list;
    }

    public void addModeInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Stock Mode: " + (requestWhenEmpty ? "full refill" : "partial refill"));
    }

    public void addFilterInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Stock: ");
        boolean added = false;
        for (int i = 0; i < filter.getSizeInventory(); i++)
        {
            ItemStack stack = filter.getStackInSlot(i);
            if (stack != null)
            {
                list.add(EnumChatFormatting.GRAY + " - " + stack.getDisplayName() + " (" + stack.stackSize + ")");
                added = true;
            }
        }
        if (!added)
            list.add(EnumChatFormatting.GRAY + " - empty");
    }

    @Override
    public EnumRoutingChip getChipType()
    {
        return EnumRoutingChip.ITEMSTOCKKEEPER;
    }
}
