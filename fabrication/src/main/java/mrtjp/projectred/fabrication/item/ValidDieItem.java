package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.integration.GateType;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.List;

public class ValidDieItem extends Item {

    public ValidDieItem() {
        super(new Item.Properties()
                .tab(ProjectRedFabrication.FABRICATION_GROUP));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level world, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        super.appendHoverText(stack, world, tooltipList, tooltipFlag);

        if (stack.getTag() == null)
            return;

        CompoundTag tag = stack.getTag();

        // Blueprint data
        tooltipList.add(new TextComponent("Name: " + tag.getString("ic_name")).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("Tile count: " + tag.getInt("tilecount")).withStyle(ChatFormatting.GRAY));
        byte bmask = tag.getByte("bmask");
        tooltipList.add(new TextComponent("Input mask: " + "0x" + Integer.toHexString(bmask & 0xF)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("Output mask: " + "0x" + Integer.toHexString((bmask >> 4) & 0xF)).withStyle(ChatFormatting.GRAY));
    }

    public static ItemStack createGatePart(ItemStack die) {
        ItemStack gate = GateType.FABRICATED_GATE.makeStack();
        gate.setTag(die.getTag().copy());
        return gate;
    }
}
