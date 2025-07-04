package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.init.FabricationDataComponents;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.item.BaseGatePartItem;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;

import java.util.List;

public class FabricatedGatePartItem extends BaseGatePartItem {

    public FabricatedGatePartItem(GateType gateType) {
        super(new Item.Properties(), gateType); // No creative tab
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext tooltipContext, List<Component> tooltipList, TooltipFlag flag) {
        stack.addToTooltip(FabricationDataComponents.IC_DATA_COMPONENT_TYPE, tooltipContext, tooltipList::add, flag);
    }
}
