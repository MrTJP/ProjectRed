package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.init.FabricationDataComponents;
import mrtjp.projectred.fabrication.init.FabricationItems;
import mrtjp.projectred.fabrication.item.component.BlueprintDataComponent;
import mrtjp.projectred.fabrication.item.component.ICDataComponent;
import mrtjp.projectred.integration.GateType;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;

import java.util.List;

public class PhotomaskSetItem extends Item {

    public PhotomaskSetItem() {
        super(new Item.Properties());
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        stack.addToTooltip(FabricationDataComponents.IC_DATA_COMPONENT_TYPE, context, tooltipList::add, tooltipFlag);
    }

    @Override
    public InteractionResult onItemUseFirst(ItemStack stack, UseOnContext context) {
        // Allow creative mode players to directly obtain a Fabricated Gate
        var player = context.getPlayer();
        if (player == null || !player.isCreative()) {
            return InteractionResult.PASS;
        }

        var component = BlueprintDataComponent.getComponent(stack);
        if (component == null || !component.getICData().canFabricate()) {
            return InteractionResult.PASS;
        }

        // Create gate stack with IC Data component
        ItemStack gate = GateType.FABRICATED_GATE.makeStack();
        ICDataComponent.setComponent(gate, component.getICData());

        player.addItem(gate);
        return InteractionResult.SUCCESS;
    }

    public static ItemStack createDieStack(ItemStack photomask, int count) {
        ItemStack validDieStack = new ItemStack(FabricationItems.VALID_DIE_ITEM.get(), count);

        // Copy IC data
        var component = ICDataComponent.getComponent(photomask);
        if (component != null) {
            ICDataComponent.setComponent(validDieStack, component);
        }

        return validDieStack;
    }
}
