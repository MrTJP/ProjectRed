package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.lithography.WaferType;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.List;

public class BaseSiliconWaferItem extends Item {

    private final WaferType waferType;

    public BaseSiliconWaferItem(WaferType waferType) {
        super(new Item.Properties()
                .tab(ProjectRedFabrication.FABRICATION_GROUP));
        this.waferType = waferType;
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level world, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        super.appendHoverText(stack, world, tooltipList, tooltipFlag);

        //TODO localize
        tooltipList.add(new TextComponent("Size").append(": " + waferType.getWaferWidth() + "nm x " + waferType.getWaferHeight() + "nm").withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("Defect chance").append(": " + waferType.getDefectRatePerUnitArea()*100 + "% / nm^2").withStyle(ChatFormatting.GRAY));
    }

    public WaferType getWaferType() {
        return waferType;
    }
}
