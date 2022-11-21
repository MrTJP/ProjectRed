package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.integration.GateType;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.List;

public class PhotomaskSetItem extends Item {

    public PhotomaskSetItem() {
        super(new Item.Properties()
                .tab(ProjectRedFabrication.FABRICATION_GROUP));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level p_77624_2_, List<Component> tooltipList, TooltipFlag tooltipFlag) {

        if (stack.getTag() != null) {
            //TODO localize
            tooltipList.add(new TextComponent("Name: " + stack.getTag().getString("ic_name")).withStyle(ChatFormatting.GRAY));
            tooltipList.add(new TextComponent("Tile count: " + stack.getTag().getInt("tilecount")).withStyle(ChatFormatting.GRAY));

            byte bmask = stack.getTag().getByte("bmask");
            tooltipList.add(new TextComponent("Input mask: " + "0x" + Integer.toHexString(bmask & 0xF)).withStyle(ChatFormatting.GRAY));
            tooltipList.add(new TextComponent("Output mask: " + "0x" + Integer.toHexString((bmask >> 4) & 0xF)).withStyle(ChatFormatting.GRAY));
        }
    }

    @Override
    public InteractionResult onItemUseFirst(ItemStack stack, UseOnContext context) {

        if (stack.getTag() != null) {
            ItemStack gate = GateType.FABRICATED_GATE.makeStack();
            gate.setTag(stack.getTag());

            context.getPlayer().addItem(gate);
            return InteractionResult.SUCCESS;
        }

        return InteractionResult.PASS;
    }

    public static void transferNBTToDieItem(ItemStack photomask, ItemStack die) {
        die.setTag(photomask.getTag().copy());
    }
}
