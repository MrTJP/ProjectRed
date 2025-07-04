package mrtjp.projectred.expansion.item;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import mrtjp.projectred.expansion.init.ExpansionDataComponents;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.ItemStack;

import javax.annotation.Nullable;

public record BatteryBoxStorageComponent(
        int storedPower,
        int storedPowerRenderLevel
) {
    public static final Codec<BatteryBoxStorageComponent> CODEC = RecordCodecBuilder.create(builder -> builder.group(
            Codec.SHORT.fieldOf("storedPower").forGetter(b -> (short) b.storedPower),
            Codec.BYTE.fieldOf("storedPowerRenderLevel").forGetter(b -> (byte) b.storedPowerRenderLevel)
    ).apply(builder, (sp, sprl) -> new BatteryBoxStorageComponent(sp & 0xFFFF, sprl & 0xFF)));

    public static final StreamCodec<RegistryFriendlyByteBuf, BatteryBoxStorageComponent> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.SHORT, b -> (short) b.storedPower,
            ByteBufCodecs.BYTE, b -> (byte) b.storedPowerRenderLevel,
            (sp, sprl) -> new BatteryBoxStorageComponent(sp & 0xFFFF, sprl & 0xFF)
    );

    @Nullable
    public static BatteryBoxStorageComponent getComponent(ItemStack stack) {
        return stack.get(ExpansionDataComponents.BATTERY_BOX_STORAGE_COMPONENT_TYPE);
    }

    public static void setComponent(ItemStack stack, BatteryBoxStorageComponent component) {
        stack.set(ExpansionDataComponents.BATTERY_BOX_STORAGE_COMPONENT_TYPE, component);
    }
}
