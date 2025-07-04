package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.init.FabricationDataComponents;
import mrtjp.projectred.fabrication.item.component.ICDataComponent;
import mrtjp.projectred.integration.GateType;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;

import java.util.List;

public class ValidDieItem extends Item {

    public ValidDieItem() {
        super(new Item.Properties());
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        stack.addToTooltip(FabricationDataComponents.IC_DATA_COMPONENT_TYPE, context, tooltipList::add, tooltipFlag);
    }

    public static ItemStack createGatePart(ItemStack die) {
        ItemStack gate = GateType.FABRICATED_GATE.makeStack();

        var component = ICDataComponent.getComponent(die);
        if (component != null && component.canFabricate()) {
            ICDataComponent.setComponent(gate, component);
        }

        return gate;
    }
}
