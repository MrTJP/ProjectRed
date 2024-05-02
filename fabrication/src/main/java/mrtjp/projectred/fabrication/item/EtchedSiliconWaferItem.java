package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.init.FabricationItems;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.List;

public class EtchedSiliconWaferItem extends Item {

    public EtchedSiliconWaferItem() {
        super(new Item.Properties()
                .stacksTo(1));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level world, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        super.appendHoverText(stack, world, tooltipList, tooltipFlag);

        if (stack.getTag() == null)
            return;

        CompoundTag tag = stack.getTag();

        //TODO localize

        // Blueprint data
        tooltipList.add(Component.literal("Name: " + tag.getString("ic_name")).withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.literal("Tile count: " + tag.getInt("tilecount")).withStyle(ChatFormatting.GRAY));
        byte bmask = tag.getByte("bmask");
        tooltipList.add(Component.literal("Input mask: " + "0x" + Integer.toHexString(bmask & 0xF)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.literal("Output mask: " + "0x" + Integer.toHexString((bmask >> 4) & 0xF)).withStyle(ChatFormatting.GRAY));

        // Wafer etching data
        tooltipList.add(Component.literal("Yield: " + tag.getDouble("yield")*100 + "%").withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.literal("Defects: " + tag.getInt("defectCount")).withStyle(ChatFormatting.GRAY));
        byte[] defects = tag.getByteArray("defects");
        int gridLen = tag.getInt("gridLen");
        for (int y = 0; y < gridLen; y++) {
            StringBuilder s = new StringBuilder();
            for (int x = 0; x < gridLen; x++) {
                int i = y * gridLen + x;
                s.append(defects[i] == 0 ? "[-]" : "[X]");
            }
            tooltipList.add(Component.literal("  " + s).withStyle(ChatFormatting.GRAY));
        }
    }

    public static ItemStack createFromPhotomaskSet(ItemStack photomaskSet, int waferLen, int dieLen, double defectChancePerLen) {

        // Create copy of photomask tag
        CompoundTag tag = photomaskSet.getTag().copy();

        int gridLen = waferLen / dieLen;
        double defectChancePerDie = dieLen * defectChancePerLen;
        int totalDesigns = gridLen * gridLen;
        int totalDefects = 0;

        byte[] defects = new byte[totalDesigns];
        for (int i = 0; i < totalDesigns; i++) {
            if (Math.random() < defectChancePerDie) {
                defects[i] = 1;
                totalDefects++;
            }
        }

        double yield = (totalDesigns - totalDefects) / (double) totalDesigns;

        tag.putInt("gridLen", gridLen);
        tag.putInt("designCount", totalDesigns);
        tag.putInt("defectCount", totalDefects);
        tag.putDouble("yield", yield);
        tag.putByteArray("defects", defects);

        // Put NBT on new item stack and return
        ItemStack output = new ItemStack(FabricationItems.ETCHED_SILICON_WAFER_ITEM.get());
        output.setTag(tag);
        return output;
    }
}
