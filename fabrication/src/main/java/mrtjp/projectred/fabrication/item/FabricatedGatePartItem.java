package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.item.GatePartItem;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class FabricatedGatePartItem extends GatePartItem {

    public FabricatedGatePartItem(GateType gateType) {
        super(gateType);
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level level, List<Component> tooltipList, TooltipFlag flag) {
        ICBlueprintItem.buildTooltip(stack.getTag(), tooltipList);
    }
}
