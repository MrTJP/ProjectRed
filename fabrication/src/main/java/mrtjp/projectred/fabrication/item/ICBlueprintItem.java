package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.engine.InterfaceSpec;
import mrtjp.projectred.fabrication.init.FabricationBlocks;
import mrtjp.projectred.fabrication.init.FabricationItems;
import mrtjp.projectred.integration.GateType;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;

import static mrtjp.projectred.fabrication.editor.EditorDataUtils.*;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class ICBlueprintItem extends Item {

    public ICBlueprintItem() {
        super(new Item.Properties()
                .stacksTo(1));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level p_77624_2_, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        buildTooltip(stack.getTag(), tooltipList);
    }

    @Override
    public InteractionResult onItemUseFirst(ItemStack stack, UseOnContext context) {
        // Allow creative mode players to directly obtain a Fabricated Gate
        var player = context.getPlayer();
        if (player == null || !player.isCreative()) {
            return InteractionResult.PASS;
        }

        if (!canFabricate(stack.getTag())) {
            return InteractionResult.PASS;
        }

        // Always prioritize placing blueprint on IC workbench
        BlockState blockState = context.getLevel().getBlockState(context.getClickedPos());
        if (blockState.getBlock() == FabricationBlocks.IC_WORKBENCH_BLOCK.get()) {
            return InteractionResult.PASS;
        }

        ItemStack gate = GateType.FABRICATED_GATE.makeStack();
        gate.setTag(createFabricationCopy(Objects.requireNonNull(stack.getTag())));

        player.addItem(gate);
        return InteractionResult.SUCCESS;
    }

    public static void buildTooltip(@Nullable CompoundTag blueprintTag, List<Component> tooltipList) {

        if (blueprintTag == null) return;

        if (!hasFabricationTarget(blueprintTag)) {
            tooltipList.add(Component.literal("<!> ").withStyle(ChatFormatting.RED)
                    .append(Component.translatable(UL_CORRUPTED_DISCARD).withStyle(ChatFormatting.GRAY)));
            return;
        }

        tooltipList.add(Component.translatable(UL_NAME).append(": " + blueprintTag.getString(KEY_IC_NAME)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.translatable(UL_AUTHOR, blueprintTag.getString(KEY_IC_AUTHOR)).withStyle(ChatFormatting.GRAY, ChatFormatting.ITALIC));
        tooltipList.add(Component.translatable(UL_TILE_COUNT).append(": " + blueprintTag.getInt(KEY_TILE_COUNT)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.translatable(UL_BLUEPRINT_DIM).append(": " + blueprintTag.getString(KEY_DIMENSIONS)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.translatable(UL_IO_TYPES).append(": ").withStyle(ChatFormatting.GRAY));

        InterfaceSpec spec = getInterfaceSpec(blueprintTag);
        Component indent = Component.literal("  ");
        tooltipList.add(indent.copy().append(Component.translatable(UL_TOP)).append(": ").append(Component.translatable(spec.getInterfaceType(0).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(indent.copy().append(Component.translatable(UL_RIGHT)).append(": ").append(Component.translatable(spec.getInterfaceType(1).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(indent.copy().append(Component.translatable(UL_BOTTOM)).append(": ").append(Component.translatable(spec.getInterfaceType(2).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(indent.copy().append(Component.translatable(UL_LEFT)).append(": ").append(Component.translatable(spec.getInterfaceType(3).getUnlocalName())).withStyle(ChatFormatting.GRAY));

        tooltipList.add(Component.translatable(UL_INPUT_MASK).append(String.format(": 0x%X", spec.getInputMask())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(Component.translatable(UL_OUTPUT_MASK).append(String.format(": 0x%X", spec.getOutputMask())).withStyle(ChatFormatting.GRAY));

        int warningCount = getWarningCount(blueprintTag);
        int errorCount = getErrorCount(blueprintTag);
        if (warningCount > 0) {
            tooltipList.add(Component.literal("<!> ").withStyle(ChatFormatting.YELLOW)
                    .append(Component.translatable(UL_UNIT_WARNINGS, warningCount).withStyle(ChatFormatting.GRAY)));
        }
        if (errorCount > 0) {
            tooltipList.add(Component.literal("<!> ").withStyle(ChatFormatting.RED)
                    .append(Component.translatable(UL_UNIT_ERRORS, errorCount).withStyle(ChatFormatting.GRAY)));
        }

        if (!isBuilt(blueprintTag)) {
            tooltipList.add(Component.literal(" - ")
                    .append(Component.translatable(UL_FAB_ERR_NOT_COMPILED)).withStyle(ChatFormatting.RED));
        }

        if (!isCompileFormatValid(blueprintTag)) {
            tooltipList.add(Component.literal(" - ")
                    .append(Component.translatable(UL_FAB_ERR_COMPILE_FORMAT)).withStyle(ChatFormatting.RED));
        }

        if (!canFabricate(blueprintTag)) {
            tooltipList.add(Component.literal(" - ")
                    .append(Component.translatable(UL_FAB_ERR_GENERIC)).withStyle(ChatFormatting.RED));
        }
    }

    private static Component getBundledIOTextComponent(byte bmask, int r) {
        int i = 0x01 << r;
        int o = 0x10 << r;
        return Component.translatable((bmask & i) != 0 ? UL_BUNDLED_INPUT : (bmask & o) != 0 ? UL_BUNDLED_OUTPUT : UL_IO_NONE);
    }

    public static ItemStack createPhotomaskStack(ItemStack blueprintStack) {

        ItemStack photomaskStack = new ItemStack(FabricationItems.PHOTOMASK_SET_ITEM.get());
        CompoundTag blueprintTag = blueprintStack.getTag();

        if (!hasFabricationTarget(blueprintTag)) return photomaskStack;

        CompoundTag photomaskTag = createFabricationCopy(blueprintTag);
        photomaskStack.setTag(photomaskTag);

        return photomaskStack;
    }
}
