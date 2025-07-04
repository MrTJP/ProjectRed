package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.lithography.WaferType;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;

import java.util.List;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_DEFECT_CHANCE;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_SIZE;

public class BaseSiliconWaferItem extends Item {

    private final WaferType waferType;

    public BaseSiliconWaferItem(WaferType waferType) {
        super(new Item.Properties());
        this.waferType = waferType;
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        super.appendHoverText(stack, context, tooltipList, tooltipFlag);

        tooltipList.add(Component.translatable(UL_SIZE).append(": " + waferType.getWaferWidth() + "nm x " + waferType.getWaferHeight() + "nm").withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.translatable(UL_DEFECT_CHANCE).append(": %.2e%% / nm^2".formatted(waferType.getDefectRatePerUnitArea()*100)).withStyle(ChatFormatting.GRAY));
    }

    public WaferType getWaferType() {
        return waferType;
    }
}
