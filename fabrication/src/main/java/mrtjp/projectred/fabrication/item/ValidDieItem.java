package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.integration.GateType;
import net.minecraft.network.chat.Component;
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
        ICBlueprintItem.buildTooltip(stack.getTag(), tooltipList);
    }

    public static ItemStack createGatePart(ItemStack die) {
        ItemStack gate = GateType.FABRICATED_GATE.makeStack();
        gate.setTag(die.getTag().copy());
        return gate;
    }
}
