package mrtjp.projectred.expansion.item;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import mrtjp.projectred.expansion.init.ExpansionDataComponents;
import net.covers1624.quack.collection.FastStream;
import net.minecraft.ChatFormatting;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.Container;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.component.TooltipProvider;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

import static mrtjp.projectred.expansion.init.ExpansionUnlocal.UL_PLAN_RESULT;

public class RecipePlanComponent implements TooltipProvider {

    private final List<ItemStack> inputs;
    private final ItemStack output;

    private RecipePlanComponent(List<ItemStack> inputs, ItemStack output) {
        this.inputs = inputs;
        this.output = output;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof RecipePlanComponent that)) return false;
        return Objects.equals(inputs, that.inputs) && Objects.equals(output, that.output);
    }

    @Override
    public int hashCode() {
        return Objects.hash(inputs, output);
    }

    //region Component getter/setter
    @Nullable
    public static RecipePlanComponent getComponent(ItemStack stack) {
        return stack.get(ExpansionDataComponents.RECIPE_PLAN_COMPONENT_TYPE);
    }

    public static void setComponent(ItemStack stack, RecipePlanComponent component) {
        stack.set(ExpansionDataComponents.RECIPE_PLAN_COMPONENT_TYPE, component);
    }
    //endregion

    //region Data accessors
    public ItemStack[] getInputs() {
        // Return deep copy of inputs to avoid external modification
        return FastStream.of(inputs)
                .map(ItemStack::copy)
                .toArray(new ItemStack[0]);
    }

    public ItemStack getOutput() {
        // Return a copy of the output to avoid external modification
        return output.copy();
    }
    //endregion

    //region Utility methods
    public boolean isRecipeValid() {
        // To be valid, must have at least one input and a non-empty output
        return !output.isEmpty() && FastStream.of(inputs).anyMatch(i -> !i.isEmpty());
    }

    public boolean loadPlanInputsToGrid(Container craftingGrid) {
        ItemStack[] inputs = getInputs();
        if (inputs.length != craftingGrid.getContainerSize()) {
            return false;
        }

        for (int i = 0; i < inputs.length; i++) {
            // Inputs already copied, so set directly
            craftingGrid.setItem(i, inputs[i]);
        }
        return true;
    }

    public static void savePlan(ItemStack stack, ItemStack[] inputs, ItemStack output) {
        var inputsList = FastStream.of(inputs)
                .map(inputStack -> {
                    var copy = inputStack.copy();
                    if (copy.isDamageableItem()) {
                        // Always save without damage
                        copy.setDamageValue(0);
                    }
                    return copy;
                })
                .toList();

        RecipePlanComponent component = new RecipePlanComponent(inputsList, output.copy());
        setComponent(stack, component);
    }
    //endregion

    //region TooltipProvider
    @Override
    public void addToTooltip(Item.TooltipContext context, Consumer<Component> tooltip, TooltipFlag flag) {
        if (!isRecipeValid()) {
            //TODO localize
            tooltip.accept(Component.translatable("Damaged recipe!").withStyle(ChatFormatting.RED));
            return;
        }

        tooltip.accept(Component.translatable(UL_PLAN_RESULT).append(": " + output.getDisplayName().getString()).withStyle(ChatFormatting.GRAY));
    }
    //endregion

    //region Codecs
    // Codecs
    public static final Codec<RecipePlanComponent> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.list(ItemStack.CODEC).fieldOf("inputs").forGetter(c -> c.inputs),
            ItemStack.CODEC.fieldOf("output").forGetter(c -> c.output)
    ).apply(instance, RecipePlanComponent::new));

    public static final StreamCodec<RegistryFriendlyByteBuf, RecipePlanComponent> STREAM_CODEC = StreamCodec.composite(
            ItemStack.STREAM_CODEC.apply(ByteBufCodecs.list()), c -> c.inputs,
            ItemStack.STREAM_CODEC, c -> c.output,
            RecipePlanComponent::new
    );
    //endregion
}
