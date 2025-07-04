package mrtjp.projectred.exploration.item.component;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import mrtjp.projectred.exploration.init.ExplorationDataComponents;
import mrtjp.projectred.exploration.inventory.BackpackInventory;
import net.covers1624.quack.collection.FastStream;
import net.minecraft.ChatFormatting;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.component.TooltipProvider;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public class BackpackDataComponent implements TooltipProvider {

    private final List<ItemStack> items;
    private final boolean isOpened;
    private final int itemCount;

    private BackpackDataComponent(List<ItemStack> items, boolean isOpened) {
        this.items = items;
        this.isOpened = isOpened;
        this.itemCount = (int) items.stream().filter(stack -> !stack.isEmpty()).count();
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof BackpackDataComponent that)) return false;
        return isOpened == that.isOpened
                && itemCount == that.itemCount
                && Objects.equals(items, that.items);
    }

    @Override
    public int hashCode() {
        return Objects.hash(items, isOpened, itemCount);
    }

    //region Component getter/setter
    @Nullable
    public static BackpackDataComponent getComponent(ItemStack stack) {
        return stack.get(ExplorationDataComponents.BACKPACK_DATA_COMPONENT);
    }

    public static void setComponent(ItemStack stack, BackpackDataComponent component) {
        stack.set(ExplorationDataComponents.BACKPACK_DATA_COMPONENT, component);
    }
    //endregion

    //region Data accessors
    public boolean isOpened() {
        return isOpened;
    }

    public List<ItemStack> getItemsCopy() {
        // Return a copy of the items list
        return FastStream.of(items)
                .map(ItemStack::copy)
                .toList();
    }

    public int getItemCount() {
        // Return the count of non-empty items
        return itemCount;
    }
    //endregion

    //region Utilities
    public BackpackInventory createInventory() {
        // Create a new BackpackInventory with the items
        return new BackpackInventory(getItemsCopy());
    }

    public BackpackDataComponent withOpenedState(boolean isOpened) {
        return this.isOpened == isOpened ? this : new BackpackDataComponent(items, isOpened);
    }

    public static BackpackDataComponent fromInventory(BackpackInventory inventory, boolean isOpened) {
        // Create a BackpackDataComponent from a BackpackInventory
        var itemsCopy = FastStream.of(inventory.getItems())
                .map(ItemStack::copy)
                .toList();
        return new BackpackDataComponent(itemsCopy, isOpened);
    }

    public static BackpackDataComponent getOrCreateComponent(ItemStack stack, int size, boolean isOpened) {
        var component = getComponent(stack);

        // If not found, create one and save it
        if (component == null) {
            List<ItemStack> items = Collections.nCopies(size, ItemStack.EMPTY);
            return new BackpackDataComponent(items, isOpened);
        }

        return component.withOpenedState(isOpened);
    }
    //endregion

    //region TooltipProvider
    @Override
    public void addToTooltip(Item.TooltipContext context, Consumer<Component> tooltip, TooltipFlag flag) {
        if (isOpened) return; // Don't show tooltip if backpack is opened
        tooltip.accept(Component.literal(itemCount + " / 27").withStyle(ChatFormatting.GRAY));
    }
    //endregion

    //region Codecs
    public static final Codec<BackpackDataComponent> CODEC = RecordCodecBuilder.create(builder -> builder.group(
            Codec.list(ItemStack.OPTIONAL_CODEC).fieldOf("items").forGetter(b -> b.items),
            Codec.BOOL.fieldOf("isOpened").forGetter(b -> b.isOpened)
    ).apply(builder, BackpackDataComponent::new));

    public static final StreamCodec<RegistryFriendlyByteBuf, BackpackDataComponent> STREAM_CODEC = StreamCodec.composite(
            ItemStack.OPTIONAL_LIST_STREAM_CODEC, b -> b.items,
            ByteBufCodecs.BOOL, b -> b.isOpened,
            BackpackDataComponent::new
    );
    //endregion
}
