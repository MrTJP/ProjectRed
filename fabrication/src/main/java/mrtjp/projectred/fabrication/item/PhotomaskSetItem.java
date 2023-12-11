package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.init.FabricationItems;
import mrtjp.projectred.integration.GateType;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;

public class PhotomaskSetItem extends Item {

    public PhotomaskSetItem() {
        super(new Item.Properties()
                .tab(ProjectRedFabrication.FABRICATION_GROUP));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level p_77624_2_, List<Component> tooltipList, TooltipFlag tooltipFlag) {

        ICBlueprintItem.buildTooltip(stack.getTag(), tooltipList);
    }

    @Override
    public InteractionResult onItemUseFirst(ItemStack stack, UseOnContext context) {

        if (stack.getTag() != null && context.getPlayer() != null) {
            ItemStack gate = GateType.FABRICATED_GATE.makeStack();
            gate.setTag(stack.getTag());

            context.getPlayer().addItem(gate);
            return InteractionResult.SUCCESS;
        }

        return InteractionResult.PASS;
    }

    public static ItemStack createDieStack(ItemStack photomask, int count) {
        ItemStack validDieStack = new ItemStack(FabricationItems.VALID_DIE_ITEM.get(), count);
        validDieStack.setTag(Objects.requireNonNull(photomask.getTag()).copy()); //Nothing additional to add yet
        return validDieStack;
    }
}
