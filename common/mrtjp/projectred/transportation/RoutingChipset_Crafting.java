package mrtjp.projectred.transportation;

import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;

import java.util.LinkedList;
import java.util.List;

public class RoutingChipset_Crafting extends RoutingChipset
{
    private SimpleInventory matrix = new SimpleInventory(10, "matrix", 127);

    public int extIndex[] = new int[] {-1, -1, -1, -1, -1, -1, -1, -1, -1};

    public int priority = 0;

    public boolean overridePipePriority = false;

    public IInventory getMatrix()
    {
        return matrix;
    }

    public int getMaxExtensions()
    {
        return getUpgradeBus().RLatency();
    }

    public int getMaxPriority()
    {
        return getUpgradeBus().LLatency();
    }

    public void priorityUp()
    {
        priority = Math.min(getMaxPriority(), ++priority);
    }

    public void priorityDown()
    {
        priority = Math.max(-getMaxPriority(), --priority);
    }

    public void extUp(int index)
    {
        if (index > -1 && index < 9)
            extIndex[index] = Math.min(extIndex[index]+1, 8);
    }

    public void extDown(int index)
    {
        if (index > -1 && index < 9)
            extIndex[index] = Math.max(extIndex[index] - 1, -1);
    }

    @Override
    public UpgradeBus createUpgradeBus()
    {
        UpgradeBus b = new UpgradeBus(3, 3);
        b.setLatency(2, 6, 8, 1, 3, 5);

        b.Linfo = "raise max priority value";
        b.Lformula = "priority value = Latency";

        b.Rinfo = "number of extensions";
        b.Rformula = "extensions = Latency";
        return b;
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        matrix.save(tag);
        tag.setIntArray("ext", extIndex);
        tag.setBoolean("over", overridePipePriority);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        matrix.load(tag);
        extIndex = tag.getIntArray("ext");
        overridePipePriority = tag.getBoolean("over");
    }

    @Override
    public List<String> infoCollection()
    {
        List<String> list = new LinkedList<String>();
        addMatrixInfo(list);
        if (getMaxPriority() > 0)
            addPriorityInfo(list);
        if (getMaxExtensions() > 0)
            addExtInfo(list);
        return list;
    }

    public void addMatrixInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Matrix: ");
        boolean added = false;
        for (int i = 0; i < 9; i++)
        {
            ItemStack stack = matrix.getStackInSlot(i);
            if (stack != null)
            {
                list.add(EnumChatFormatting.GRAY + " - " + stack.getDisplayName() + " (" + stack.stackSize + ")");
                added = true;
            }
        }
        if (!added)
            list.add(EnumChatFormatting.GRAY + " - empty");

        ItemStack stack = matrix.getStackInSlot(9);
        if (stack != null)
            list.add(EnumChatFormatting.GRAY + " - Yields: " + stack.getDisplayName() + " (" + stack.stackSize + ")");
    }

    public void addExtInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Extensions:");
        for (int i = 0; i < 3; i++)
        {
            String s = EnumChatFormatting.GRAY.toString()+" - ";
            for (int j = 0; j < 3; j++)
                s = s + "[" + (extIndex[j+(i*3)] >= 0 ? "+" : "-") + "]";
            list.add(s);
        }
    }

    public void addPriorityInfo(List<String> list)
    {
        list.add(EnumChatFormatting.GRAY + "Priority: " + priority);
        list.add(EnumChatFormatting.GRAY + "Priority override: " + (overridePipePriority ? "yes" : "no"));
    }

    @Override
    public EnumRoutingChip getChipType()
    {
        return EnumRoutingChip.ITEMCRAFTING;
    }
}
