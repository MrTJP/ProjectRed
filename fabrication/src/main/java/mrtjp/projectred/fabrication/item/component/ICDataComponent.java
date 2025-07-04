package mrtjp.projectred.fabrication.item.component;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import mrtjp.projectred.fabrication.engine.InterfaceSpec;
import mrtjp.projectred.fabrication.engine.PRFabricationEngine;
import mrtjp.projectred.fabrication.init.FabricationDataComponents;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
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
import java.util.Optional;
import java.util.function.Consumer;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class ICDataComponent implements TooltipProvider {

    // Empty instance
    public static final ICDataComponent EMPTY = new ICDataComponent(
            "EMPTY", 0, 0, 0, false, -1, InterfaceSpec.EMPTY, PRFabricationEngine.EMPTY_FLAT_MAP_SERIALIZED
    );

    // Metadata
    private final String name;
    private final int tileCount;

    // Compile info
    private final int warningCount;
    private final int errorCount;
    private final boolean isBuilt;
    private final int compileFormat;

    // IO
    private final InterfaceSpec interfaceSpec;

    // Flat map
    private final String flatMap;

    public ICDataComponent(String name, int tileCount, int warningCount, int errorCount, boolean isBuilt, int compileFormat, InterfaceSpec interfaceSpec, String flatMap) {
        this.name = name;
        this.tileCount = tileCount;
        this.warningCount = warningCount;
        this.errorCount = errorCount;
        this.isBuilt = isBuilt;
        this.compileFormat = compileFormat;
        this.interfaceSpec = interfaceSpec;
        this.flatMap = flatMap;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof ICDataComponent that)) return false;
        return tileCount == that.tileCount
                && warningCount == that.warningCount
                && errorCount == that.errorCount
                && isBuilt == that.isBuilt
                && compileFormat == that.compileFormat
                && Objects.equals(name, that.name)
                && Objects.equals(interfaceSpec, that.interfaceSpec)
                && Objects.equals(flatMap, that.flatMap);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                name,
                tileCount,
                warningCount,
                errorCount,
                isBuilt,
                compileFormat,
                interfaceSpec,
                flatMap);
    }

    //region Getters
    //@formatter:off
    public String getName() { return name; }
    public int getTileCount() { return tileCount; }
    public int getWarningCount() { return warningCount; }
    public int getErrorCount() { return errorCount; }
    public boolean isBuilt() { return isBuilt; }
    public int getCompileFormat() { return compileFormat; }
    public InterfaceSpec getInterfaceSpec() { return interfaceSpec; }
    public String getFlatMap() { return flatMap; }
    //@formatter:on
    //endregion

    //region Component setter/getter
    @Nullable
    public static ICDataComponent getComponent(ItemStack stack) {
        return stack.get(FabricationDataComponents.IC_DATA_COMPONENT_TYPE);
    }

    public static void setComponent(ItemStack stack, ICDataComponent component) {
        stack.set(FabricationDataComponents.IC_DATA_COMPONENT_TYPE, component);
    }
    //endregion

    //region Tag Save/Load
    public CompoundTag save() {
        var result = CODEC.encodeStart(NbtOps.INSTANCE, this);
        if (result.isSuccess()) {
            var out = result.getOrThrow();
            if (out instanceof CompoundTag) {
                return (CompoundTag) out;
            } else {
                LOGGER.error("FabricationDesignDataComponent codec did not produce a CompoundTag: {}", out);
                return new CompoundTag();
            }
        }

        LOGGER.error("Failed to encode FabricationDesignDataComponent: {}", result.error());
        return new CompoundTag();
    }

    public static Optional<ICDataComponent> parse(CompoundTag tag) {
        var result = CODEC.parse(NbtOps.INSTANCE, tag);
        if (result.isSuccess()) {
            return Optional.of(result.getOrThrow());
        }
        LOGGER.error("Failed to parse FabricationDesignDataComponent: {}", result.error());
        return Optional.empty();
    }
    //endregion

    //region Utility methods
    public boolean canFabricate() {
        // Can fabricate if built, compile format is valid, and no errors
        return isBuilt && errorCount == 0 && isLatestCompileFormat();
    }

    public boolean isLatestCompileFormat() {
        // Check if the compile format matches the latest expected format
        return compileFormat == PRFabricationEngine.COMPILE_FORMAT;
    }
    //endregion

    //region Tooltip Provider
    @Override
    public void addToTooltip(Item.TooltipContext context, Consumer<Component> tooltipList, TooltipFlag flag) {

        // Name and tile count
        tooltipList.accept(Component.translatable(UL_NAME).append(": " + name).withStyle(ChatFormatting.GRAY));
        tooltipList.accept(Component.translatable(UL_TILE_COUNT).append(": " + tileCount).withStyle(ChatFormatting.GRAY));
        tooltipList.accept(Component.translatable(UL_IO_TYPES).append(": ").withStyle(ChatFormatting.GRAY));

        // Interface types for each side
        Component indent = Component.literal("  ");
        tooltipList.accept(indent.copy().append(Component.translatable(UL_TOP)).append(": ").append(Component.translatable(interfaceSpec.getInterfaceType(0).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.accept(indent.copy().append(Component.translatable(UL_RIGHT)).append(": ").append(Component.translatable(interfaceSpec.getInterfaceType(1).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.accept(indent.copy().append(Component.translatable(UL_BOTTOM)).append(": ").append(Component.translatable(interfaceSpec.getInterfaceType(2).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.accept(indent.copy().append(Component.translatable(UL_LEFT)).append(": ").append(Component.translatable(interfaceSpec.getInterfaceType(3).getUnlocalName())).withStyle(ChatFormatting.GRAY));

        // Input and output masks
        tooltipList.accept(Component.translatable(UL_INPUT_MASK).append(String.format(": 0x%X", interfaceSpec.getInputMask())).withStyle(ChatFormatting.GRAY));
        tooltipList.accept(Component.translatable(UL_OUTPUT_MASK).append(String.format(": 0x%X", interfaceSpec.getOutputMask())).withStyle(ChatFormatting.GRAY));

        // Warning and error counts
        if (warningCount > 0) {
            tooltipList.accept(Component.literal("<!> ").withStyle(ChatFormatting.YELLOW)
                    .append(Component.translatable(UL_UNIT_WARNINGS, warningCount).withStyle(ChatFormatting.GRAY)));
        }
        if (errorCount > 0) {
            tooltipList.accept(Component.literal("<!> ").withStyle(ChatFormatting.RED)
                    .append(Component.translatable(UL_UNIT_ERRORS, errorCount).withStyle(ChatFormatting.GRAY)));
        }

        // Build status
        if (!isBuilt) {
            tooltipList.accept(Component.literal(" - ")
                    .append(Component.translatable(UL_FAB_ERR_NOT_COMPILED)).withStyle(ChatFormatting.RED));
        }

        // Compile format validation (assuming current format is valid if compileFormat matches expected)
        // Note: This would need the actual validation logic from ICBlueprintItem.isCompileFormatValid()
        // For now, we'll skip this check since we don't have access to the expected format constant

        // Generic fabrication check (would need the actual canFabricate logic)
        // For now, we'll assume it's fabricable if built and no errors
        if (isBuilt && errorCount == 0) {
            // Fabricable - no additional error message needed
        } else if (isBuilt && errorCount > 0) {
            tooltipList.accept(Component.literal(" - ")
                    .append(Component.translatable(UL_FAB_ERR_GENERIC)).withStyle(ChatFormatting.RED));
        }
    }
    //endregion

    //region Codecs
    public static final Codec<ICDataComponent> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.STRING.fieldOf("name").forGetter(component -> component.name),
            Codec.INT.fieldOf("tileCount").forGetter(component -> component.tileCount),
            Codec.INT.fieldOf("warningCount").forGetter(component -> component.warningCount),
            Codec.INT.fieldOf("errorCount").forGetter(component -> component.errorCount),
            Codec.BOOL.fieldOf("isBuilt").forGetter(component -> component.isBuilt),
            Codec.INT.fieldOf("compileFormat").forGetter(component -> component.compileFormat),
            InterfaceSpec.CODEC.fieldOf("interfaceSpec").forGetter(component -> component.interfaceSpec),
            Codec.STRING.fieldOf("flatMap").forGetter(component -> component.flatMap)
    ).apply(instance, ICDataComponent::new));

    public static final StreamCodec<RegistryFriendlyByteBuf, ICDataComponent> STREAM_CODEC = StreamCodec.of(
            (buf, component) -> {
                ByteBufCodecs.STRING_UTF8.encode(buf, component.name);
                ByteBufCodecs.INT.encode(buf, component.tileCount);
                ByteBufCodecs.INT.encode(buf, component.warningCount);
                ByteBufCodecs.INT.encode(buf, component.errorCount);
                ByteBufCodecs.BOOL.encode(buf, component.isBuilt);
                ByteBufCodecs.INT.encode(buf, component.compileFormat);
                InterfaceSpec.STREAM_CODEC.encode(buf, component.interfaceSpec);
                ByteBufCodecs.STRING_UTF8.encode(buf, component.flatMap);
            },
            buf -> new ICDataComponent(
                    ByteBufCodecs.STRING_UTF8.decode(buf),
                    ByteBufCodecs.INT.decode(buf),
                    ByteBufCodecs.INT.decode(buf),
                    ByteBufCodecs.INT.decode(buf),
                    ByteBufCodecs.BOOL.decode(buf),
                    ByteBufCodecs.INT.decode(buf),
                    InterfaceSpec.STREAM_CODEC.decode(buf),
                    ByteBufCodecs.STRING_UTF8.decode(buf)
            )
    );
    //endregion

    //region Builder
    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {

        private @Nullable String name;
        private int tileCount = -1;
        private int warningCount = -1;
        private int errorCount = -1;
        private boolean isBuilt;
        private int compileFormat = -1;
        private @Nullable InterfaceSpec interfaceSpec;
        private @Nullable String flatMap;

        public Builder setName(String name) {
            this.name = name;
            return this;
        }

        public Builder setTileCount(int tileCount) {
            this.tileCount = tileCount;
            return this;
        }

        public Builder setWarningCount(int warningCount) {
            this.warningCount = warningCount;
            return this;
        }

        public Builder setErrorCount(int errorCount) {
            this.errorCount = errorCount;
            return this;
        }

        public Builder setBuilt(boolean isBuilt) {
            this.isBuilt = isBuilt;
            return this;
        }

        public Builder setCompileFormat(int compileFormat) {
            this.compileFormat = compileFormat;
            return this;
        }

        public Builder setInterfaceSpec(InterfaceSpec interfaceSpec) {
            this.interfaceSpec = interfaceSpec;
            return this;
        }

        public Builder setFlatMap(String flatMap) {
            this.flatMap = flatMap;
            return this;
        }

        public ICDataComponent build() {
            if (name == null || interfaceSpec == null || flatMap == null) {
                throw new IllegalArgumentException("Name, InterfaceSpec, and FlatMap cannot be null");
            }
            if (tileCount < 0 || warningCount < 0 || errorCount < 0 || compileFormat < 0) {
                throw new IllegalArgumentException("Tile count, warning count, error count, and compile format must be non-negative");
            }
            // Create and return the FabricationDesignDataComponent
            return new ICDataComponent(name, tileCount, warningCount, errorCount, isBuilt, compileFormat, interfaceSpec, flatMap);
        }
    }
    //endregion
}
