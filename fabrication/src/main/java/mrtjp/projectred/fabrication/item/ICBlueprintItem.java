package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.init.FabricationBlocks;
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
import net.minecraft.world.level.block.state.BlockState;

import java.util.List;

public class ICBlueprintItem extends Item {

    public ICBlueprintItem() {
        super(new Item.Properties()
                .stacksTo(1));
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        stack.addToTooltip(FabricationDataComponents.BLUEPRINT_DATA_COMPONENT_TYPE, context, tooltipList::add, tooltipFlag);
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

        // Always prioritize placing blueprint on IC workbench
        BlockState blockState = context.getLevel().getBlockState(context.getClickedPos());
        if (blockState.getBlock() == FabricationBlocks.IC_WORKBENCH_BLOCK.get()) {
            return InteractionResult.PASS;
        }

        // Create gate stack with IC Data component
        ItemStack gate = GateType.FABRICATED_GATE.makeStack();
        ICDataComponent.setComponent(gate, component.getICData());

        player.addItem(gate);
        return InteractionResult.SUCCESS;
    }

    public static ItemStack createPhotomaskStack(ItemStack blueprintStack) {
        var component = BlueprintDataComponent.getComponent(blueprintStack);
        ItemStack photomaskStack = new ItemStack(FabricationItems.PHOTOMASK_SET_ITEM.get());

        if (component == null || !component.getICData().canFabricate()) {
            // If blueprint is invalid, return empty photomask stack
            return photomaskStack;
        }

        // Copy the IC data to the photomask stack
        ICDataComponent.setComponent(photomaskStack, component.getICData());
        return photomaskStack;
    }
}
