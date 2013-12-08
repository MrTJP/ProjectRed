package mrtjp.projectred.transportation;

import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;

import org.lwjgl.input.Keyboard;

public class RoutingChipset_ItemResponder extends RoutingChipset
{
    public SimpleInventory filter = new SimpleInventory(9, "filter", 1);

    private final SendPriority priority = getSendPriority();
    public int preference = 0;

    public boolean filterExclude = false;
    public boolean fuzzyMode = false;
    public int fuzzyDamageMode = 0;
    public static final int[] fuzzyPercent = new int[] {0, 25, 50, 75, 100};

    public void prefUp() {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            preference += 10;
        else
            preference += 1;
        if (preference > 100)
            preference = 100;
    }

    public void prefDown() {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            preference -= 10;
        else
            preference -= 1;
        if (preference < -100)
            preference = -100;
    }
    
    public void shiftFuzzy() {
        fuzzyDamageMode = (fuzzyDamageMode + 1) % 5;
    }

    @Override
    public SyncResponse getSyncResponse(ItemKey item, SyncResponse rival) {
        IInventory real = inventoryProvider().getInventory();
        int side = inventoryProvider().getInterfacedSide();

        if (real == null || side < 0)
            return null;

        if (priority.ordinal() > rival.priority.ordinal() || priority.ordinal() == rival.priority.ordinal() && preference > rival.customPriority) {
            InventoryWrapper filt = InventoryWrapper.wrapInventory(filter).setSlotsAll()
                    .setFuzzy(fuzzyMode).setFuzzyPercent(fuzzyPercent[fuzzyDamageMode]);

            if (filt.hasItem(item) != filterExclude) {
                InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);
                int room = inv.getRoomAvailableForItem(item);
                if (room > 0)
                    return new SyncResponse().setPriority(priority).setCustomPriority(preference).setItemCount(room);
            }
        }
        return null;
    }

    @Override
    public void save(NBTTagCompound tag) {
        filter.save(tag);
        tag.setInteger("pref", preference);
        tag.setBoolean("mode", filterExclude);
        tag.setBoolean("fuz", fuzzyMode);
        tag.setByte("fuzd", (byte) fuzzyDamageMode);
    }

    @Override
    public void load(NBTTagCompound tag) {
        filter.load(tag);
        preference = tag.getInteger("pref");
        filterExclude = tag.getBoolean("mode");
        fuzzyMode = tag.getBoolean("fuz");
        fuzzyDamageMode = tag.getByte("fuzd");
    }

    @Override
    public List<String> infoCollection() {
        List<String> list = new LinkedList<String>();
        addPriorityInfo(list);
        addFilterInfo(list);
        return list;
    }

    public void addPriorityInfo(List<String> list) {
        list.add(EnumChatFormatting.GRAY + "Preference: " + preference);
    }
    public void addFilterInfo(List<String> list) {
        list.add(EnumChatFormatting.GRAY + "Fuzzy Mode: " + fuzzyMode);
        list.add(EnumChatFormatting.GRAY + "Fuzzy Tool Damage: " + fuzzyPercent[fuzzyDamageMode] + "%");
        list.add(EnumChatFormatting.GRAY + "Filter Mode: " + (filterExclude ? "blacklist" : "whitelist"));
        list.add(EnumChatFormatting.GRAY + "Filter: ");
        boolean added = false;
        for (int i = 0; i < filter.getSizeInventory(); i++) {
            ItemStack stack = filter.getStackInSlot(i);
            if (stack != null) {
                list.add(EnumChatFormatting.GRAY + " - " + stack.getDisplayName());
                added = true;
            }
        }
        if (!added)
            list.add(EnumChatFormatting.GRAY + " - empty");
    }
    
    protected SendPriority getSendPriority() {
        return SendPriority.PASSIVE;
    }
    
    @Override
    public EnumRoutingChip getChipType() {
        return EnumRoutingChip.ITEMRESPONDER;
    }
}
