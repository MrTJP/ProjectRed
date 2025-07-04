package mrtjp.projectred.expansion.item;

import mrtjp.projectred.expansion.init.ExpansionDataComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;

import java.util.List;

public class RecipePlanItem extends Item {

    public RecipePlanItem() {
        super(new Item.Properties());
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> tooltip, TooltipFlag flag) {
        stack.addToTooltip(ExpansionDataComponents.RECIPE_PLAN_COMPONENT_TYPE, context, tooltip::add, flag);
    }
}
