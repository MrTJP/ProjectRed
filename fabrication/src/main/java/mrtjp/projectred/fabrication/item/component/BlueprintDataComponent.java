package mrtjp.projectred.fabrication.item.component;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import mrtjp.projectred.fabrication.init.FabricationDataComponents;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.component.TooltipProvider;

import javax.annotation.Nullable;
import java.util.Objects;
import java.util.function.Consumer;

public class BlueprintDataComponent implements TooltipProvider {

    // Editor full compound tag
    private final CompoundTag editorTag;

    // Design data
    private final ICDataComponent designData;

    //Note: DO NOT modify editorTag after construction. Copy it beforehand if you need to.
    public BlueprintDataComponent(CompoundTag editorTag, ICDataComponent designData) {
        this.editorTag = editorTag;
        this.designData = designData;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof BlueprintDataComponent that)) return false;
        return Objects.equals(editorTag, that.editorTag) && Objects.equals(designData, that.designData);
    }

    @Override
    public int hashCode() {
        return Objects.hash(editorTag, designData);
    }

    //region Getters
    public CompoundTag getEditorTag() {
        // Return a copy to ensure immutability
        return editorTag.copy();
    }

    public ICDataComponent getICData() {
        return designData; // Immutable, no need to copy
    }
    //endregion

    //region Component setter/getter
    @Nullable
    public static BlueprintDataComponent getComponent(ItemStack stack) {
        return stack.get(FabricationDataComponents.BLUEPRINT_DATA_COMPONENT_TYPE);
    }
    public static void setComponent(ItemStack stack, BlueprintDataComponent component) {
        stack.set(FabricationDataComponents.BLUEPRINT_DATA_COMPONENT_TYPE, component);
    }
    //endregion

    //region Tooltip
    @Override
    public void addToTooltip(Item.TooltipContext context, Consumer<Component> component, TooltipFlag flag) {
        designData.addToTooltip(context, component, flag);
    }
    //endregion

    //region Codecs
    public static final Codec<BlueprintDataComponent> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            CompoundTag.CODEC.fieldOf("editorTag").forGetter(component -> component.editorTag),
            ICDataComponent.CODEC.fieldOf("designData").forGetter(component -> component.designData)
    ).apply(instance, BlueprintDataComponent::new));

    public static final StreamCodec<RegistryFriendlyByteBuf, BlueprintDataComponent> STREAM_CODEC = StreamCodec.of(
            (buf, component) -> {
                ByteBufCodecs.COMPOUND_TAG.encode(buf, component.editorTag);
                ICDataComponent.STREAM_CODEC.encode(buf, component.designData);
            },
            buf -> new BlueprintDataComponent(
                    ByteBufCodecs.COMPOUND_TAG.decode(buf),
                    ICDataComponent.STREAM_CODEC.decode(buf)
            )
    );
    //endregion
}
