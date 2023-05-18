package mrtjp.projectred.expansion.item;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import net.minecraft.client.util.ITooltipFlag;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;

import static mrtjp.projectred.expansion.init.ExpansionUnlocal.UL_PLAN_RESULT;

public class RecipePlanItem extends Item {

    public RecipePlanItem() {
        super(new Item.Properties().tab(ProjectRedExpansion.EXPANSION_GROUP));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable World world, List<ITextComponent> tooltip, ITooltipFlag flag) {

        if (RecipePlanItem.hasRecipeInside(stack)) {
            ItemStack output = RecipePlanItem.loadPlanOutput(stack);
            tooltip.add(new TranslationTextComponent(UL_PLAN_RESULT).append(": " + output.getDisplayName().getString()).withStyle(TextFormatting.GRAY));
        }
    }

    public static boolean hasRecipeInside(ItemStack stack) {
        return stack.hasTag() && stack.getTag().contains("planInputs") && stack.getTag().contains("planOutput");
    }

    public static void savePlan(ItemStack stack, ItemStack[] inputs, ItemStack output) {
        CompoundNBT inputsNBT = new CompoundNBT();
        for (int i = 0; i < 9; i++) {
            ItemStack input = inputs[i];
            if (!input.isEmpty()) {
                if (input.isDamageableItem()) { // always save without damage
                    input = input.copy();
                    input.setDamageValue(0);
                }

                CompoundNBT itemStackNBT = new CompoundNBT();
                input.save(itemStackNBT);
                inputsNBT.put("input_" + i, itemStackNBT);
            }
        }

        CompoundNBT outputNBT = new CompoundNBT();
        output.save(outputNBT);

        stack.getOrCreateTag().put("planInputs", inputsNBT);
        stack.getOrCreateTag().put("planOutput", outputNBT);
    }

    public static void loadPlanInputsToGrid(IInventory craftingGrid, ItemStack stack) {

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

        CompoundNBT inputsNBT = stack.getTag().getCompound("planInputs");
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
        return ItemStack.of(stack.getTag().getCompound("planOutput"));
    }
}
