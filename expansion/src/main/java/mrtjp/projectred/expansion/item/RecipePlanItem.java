package mrtjp.projectred.expansion.item;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.Container;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static mrtjp.projectred.expansion.init.ExpansionUnlocal.UL_PLAN_RESULT;

public class RecipePlanItem extends Item {

    public RecipePlanItem() {
        super(new Item.Properties().tab(ProjectRedExpansion.EXPANSION_GROUP));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level world, List<Component> tooltip, TooltipFlag flag) {

        if (RecipePlanItem.hasRecipeInside(stack)) {
            ItemStack output = RecipePlanItem.loadPlanOutput(stack);
            tooltip.add(Component.translatable(UL_PLAN_RESULT).append(": " + output.getDisplayName().getString()).withStyle(ChatFormatting.GRAY));
        }
    }

    public static boolean hasRecipeInside(ItemStack stack) {
        return stack.hasTag() && Objects.requireNonNull(stack.getTag()).contains("planInputs") && stack.getTag().contains("planOutput");
    }

    public static void savePlan(ItemStack stack, ItemStack[] inputs, ItemStack output) {
        CompoundTag inputsNBT = new CompoundTag();
        for (int i = 0; i < 9; i++) {
            ItemStack input = inputs[i];
            if (!input.isEmpty()) {
                if (input.isDamageableItem()) { // always save without damage
                    input = input.copy();
                    input.setDamageValue(0);
                }

                CompoundTag itemStackNBT = new CompoundTag();
                input.save(itemStackNBT);
                inputsNBT.put("input_" + i, itemStackNBT);
            }
        }

        CompoundTag outputNBT = new CompoundTag();
        output.save(outputNBT);

        stack.getOrCreateTag().put("planInputs", inputsNBT);
        stack.getOrCreateTag().put("planOutput", outputNBT);
    }

    public static void loadPlanInputsToGrid(Container craftingGrid, ItemStack stack) {

        if (!hasRecipeInside(stack)) {
            return;
        }

        ItemStack[] inputs = loadPlanInputs(stack);
        if (inputs.length != craftingGrid.getContainerSize()) {
            return;
        }

        for (int i = 0; i < inputs.length; i++) {
            craftingGrid.setItem(i, inputs[i]);
        }
    }

    public static ItemStack[] loadPlanInputs(ItemStack stack) {
        ItemStack[] inputs = new ItemStack[9];
        if (!hasRecipeInside(stack)) {
            Arrays.fill(inputs, ItemStack.EMPTY);
            return inputs;
        }
        assert stack.getTag() != null;

        CompoundTag inputsNBT = stack.getTag().getCompound("planInputs");
        for (int i = 0; i < 9; i++) {
            String id = "input_" + i;
            inputs[i] = inputsNBT.contains(id) ? ItemStack.of(inputsNBT.getCompound(id)) : ItemStack.EMPTY;
        }
        return inputs;
    }

    public static ItemStack loadPlanOutput(ItemStack stack) {
        if (!hasRecipeInside(stack)) {
            return ItemStack.EMPTY;
        }
        assert stack.getTag() != null;

        return ItemStack.of(stack.getTag().getCompound("planOutput"));
    }
}
