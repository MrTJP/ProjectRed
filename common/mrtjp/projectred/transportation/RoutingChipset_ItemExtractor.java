package mrtjp.projectred.transportation;

import java.util.BitSet;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;

public class RoutingChipset_ItemExtractor extends RoutingChipset
{
    public SimpleInventory filter = new SimpleInventory(9, "filter", 1);
    public int extractOrient = -1;
    public boolean filterExclude = false;

    public boolean fuzzyMode = false;
    public int fuzzyDamageMode = 0;
    public static final int[] fuzzyPercent = new int[] { 0, 25, 50, 75, 100 };

    private int remainingDelay = operationDelay();

    private int operationDelay()
    {
        return 100 - getUpgradeBus().LLatency();
    }

    private int itemsToExtract()
    {
        return 8 + getUpgradeBus().RLatency();
    }

    public void setOrient(int orient)
    {
        extractOrient = orient;
    }

    public void shiftFuzzy()
    {
        fuzzyDamageMode = (fuzzyDamageMode + 1) % 5;
    }

    @Override
    public void update()
    {
        if (--remainingDelay > 0)
            return;
        remainingDelay = operationDelay();

        IInventory real = inventoryProvider().getInventory();
        if (real == null)
            return;
        int side = extractOrient == -1 ? inventoryProvider().getInterfacedSide() : extractOrient;

        InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);
        InventoryWrapper filt = InventoryWrapper.wrapInventory(filter).setSlotsAll().setFuzzy(fuzzyMode).setFuzzyPercent(fuzzyPercent[fuzzyDamageMode]);

        Map<ItemKey, Integer> available = inv.getAllItemStacks();

        for (Entry<ItemKey, Integer> items : available.entrySet())
        {
            ItemKey stackKey = items.getKey();
            int stackSize = items.getValue();

            if (stackKey == null || filt.hasItem(stackKey) == filterExclude)
                continue;

            BitSet exclusions = new BitSet();
            SyncResponse s = routeLayer().getLogisticPath(stackKey, exclusions, true);
            if (s == null)
                continue;

            int leftInRun = itemsToExtract();

            while (s != null)
            {
                int toExtract = Math.min(leftInRun, stackSize);
                toExtract = Math.min(toExtract, stackKey.makeStack(0).getMaxStackSize());
                if (s.itemCount > 0)
                    toExtract = Math.min(toExtract, s.itemCount);

                if (toExtract <= 0)
                    break;

                ItemStack stack2 = stackKey.makeStack(0);

                stack2.stackSize = inv.extractItem(stackKey, toExtract);
                if (stack2.stackSize == 0)
                    break;

                routeLayer().queueStackToSend(stack2, inventoryProvider().getInterfacedSide(), s);

                leftInRun -= stack2.stackSize;
                if (leftInRun <= 0)
                    break;

                exclusions.set(s.responder);
                s = routeLayer().getLogisticPath(stackKey, exclusions, true);
            }
            return;
        }

    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        filter.save(tag);
        tag.setBoolean("mode", filterExclude);
        tag.setInteger("orient", extractOrient);
        tag.setBoolean("fuz", fuzzyMode);
        tag.setByte("fuzd", (byte) fuzzyDamageMode);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        filter.load(tag);
        filterExclude = tag.getBoolean("mode");
        extractOrient = tag.getInteger("orient");
        fuzzyMode = tag.getBoolean("fuz");
        fuzzyDamageMode = tag.getByte("fuzd");
    }

    public static final String[] dirs = new String[] { "Down", "Up", "North", "South", "West", "East" };

    @Override
    public List<String> infoCollection()
    {
        List<String> list = new LinkedList<String>();
        addUpgradeBusInfo(list);
        addOrientInfo(list);
        addFilterInfo(list);
        return list;
    }

    public void addOrientInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Extract Orientation: " + (extractOrient == -1 ? "Default" : dirs[extractOrient]));
    }

    public void addFilterInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Fuzzy Mode: " + fuzzyMode);
        list.add(EnumChatFormatting.GRAY + "Fuzzy Tool Damage: " + fuzzyPercent[fuzzyDamageMode] + "%");
        list.add(EnumChatFormatting.GRAY + "Filter Mode: " + (filterExclude ? "exclude" : "include"));
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

    @Override
    public EnumRoutingChip getChipType()
    {
        return EnumRoutingChip.ITEMEXTRACTOR;
    }
    
    @Override
    public UpgradeBus createUpgradeBus()
    {
        UpgradeBus b = new UpgradeBus(3, 3);
        b.setLatency(25, 34, 40, 8, 16, 32);

        b.Linfo = "delay between extractions";
        b.Lformula = "delay = 100 - Latency";
        
        b.Rinfo = "items to extract in one operation";
        b.Rformula = "items = 8 + Latency";
        return b;
    }
}