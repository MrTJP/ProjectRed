package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.item.BaseGatePartItem;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class FabricatedGatePartItem extends BaseGatePartItem {

    public FabricatedGatePartItem(GateType gateType) {
        super(new Item.Properties(), gateType); // No creative tab
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level level, List<Component> tooltipList, TooltipFlag flag) {
        ICBlueprintItem.buildTooltip(stack.getTag(), tooltipList);
    }
}
