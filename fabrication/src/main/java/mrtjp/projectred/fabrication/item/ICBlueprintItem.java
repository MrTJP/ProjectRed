package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.engine.InterfaceSpec;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.integration.GateType;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
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
                .tab(ProjectRedFabrication.FABRICATION_GROUP)
                .stacksTo(1));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level p_77624_2_, List<Component> tooltipList, TooltipFlag tooltipFlag) {
        buildTooltip(stack.getTag(), tooltipList);
    }

    @Override
    public InteractionResult onItemUseFirst(ItemStack stack, UseOnContext context) {

        assert context.getPlayer() != null;
        if (!context.getPlayer().isCreative()) return InteractionResult.PASS;

        // Creative mode bypass: Convert blueprint directly to gate block
        BlockState blockState = context.getLevel().getBlockState(context.getClickedPos());
        if (blockState.getBlock() == FabricationReferences.IC_WORKBENCH_BLOCK) { return InteractionResult.PASS; }

        if (!canFabricate(stack.getTag())) {
            return InteractionResult.PASS;
        }

        ItemStack gate = GateType.FABRICATED_GATE.makeStack();
        gate.setTag(createFabricationCopy(Objects.requireNonNull(stack.getTag())));

        context.getPlayer().addItem(gate);
        return InteractionResult.SUCCESS;
    }

    public static void buildTooltip(@Nullable CompoundTag blueprintTag, List<Component> tooltipList) {

        if (blueprintTag == null) return;

        if (!hasFabricationTarget(blueprintTag)) {
            tooltipList.add(new TextComponent("<!> ").withStyle(ChatFormatting.RED)
                    .append(new TranslatableComponent(UL_CORRUPTED_DISCARD).withStyle(ChatFormatting.GRAY)));
            return;
        }

        tooltipList.add(new TranslatableComponent(UL_NAME).append(": " + blueprintTag.getString(KEY_IC_NAME)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TranslatableComponent(UL_TILE_COUNT).append(": " + blueprintTag.getInt(KEY_TILE_COUNT)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TranslatableComponent(UL_IO_TYPES).append(": ").withStyle(ChatFormatting.GRAY));

        InterfaceSpec spec = getInterfaceSpec(blueprintTag);
        TextComponent indent = new TextComponent("  ");
        tooltipList.add(indent.copy().append(new TranslatableComponent(UL_TOP)).append(": ").append(new TranslatableComponent(spec.getInterfaceType(0).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(indent.copy().append(new TranslatableComponent(UL_RIGHT)).append(": ").append(new TranslatableComponent(spec.getInterfaceType(1).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(indent.copy().append(new TranslatableComponent(UL_BOTTOM)).append(": ").append(new TranslatableComponent(spec.getInterfaceType(2).getUnlocalName())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(indent.copy().append(new TranslatableComponent(UL_LEFT)).append(": ").append(new TranslatableComponent(spec.getInterfaceType(3).getUnlocalName())).withStyle(ChatFormatting.GRAY));

        tooltipList.add(new TranslatableComponent(UL_INPUT_MASK).append(String.format(": 0x%X", spec.getInputMask())).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TranslatableComponent(UL_OUTPUT_MASK).append(String.format(": 0x%X", spec.getOutputMask())).withStyle(ChatFormatting.GRAY));

        int warningCount = getWarningCount(blueprintTag);
        int errorCount = getErrorCount(blueprintTag);
        if (warningCount > 0) {
            tooltipList.add(new TextComponent("<!> ").withStyle(ChatFormatting.YELLOW)
                    .append(new TranslatableComponent(UL_UNIT_WARNINGS, warningCount).withStyle(ChatFormatting.GRAY)));
        }
        if (errorCount > 0) {
            tooltipList.add(new TextComponent("<!> ").withStyle(ChatFormatting.RED)
                    .append(new TranslatableComponent(UL_UNIT_ERRORS, errorCount).withStyle(ChatFormatting.GRAY)));
        }

        if (!canFabricate(blueprintTag)) {
            tooltipList.add(new TextComponent(" - ")
                    .append(new TranslatableComponent(UL_CANNOT_FABRICATE)).withStyle(ChatFormatting.RED));
        }
    }

    private static TranslatableComponent getBundledIOTextComponent(byte bmask, int r) {
        int i = 0x01 << r;
        int o = 0x10 << r;
        return new TranslatableComponent((bmask & i) != 0 ? UL_BUNDLED_INPUT : (bmask & o) != 0 ? UL_BUNDLED_OUTPUT : UL_IO_NONE);
    }

    public static ItemStack createPhotomaskStack(ItemStack blueprintStack) {

        ItemStack photomaskStack = new ItemStack(FabricationReferences.PHOTOMASK_SET_ITEM);
        CompoundTag blueprintTag = blueprintStack.getTag();

        if (!hasFabricationTarget(blueprintTag)) return photomaskStack;

        CompoundTag photomaskTag = createFabricationCopy(blueprintTag);
        photomaskStack.setTag(photomaskTag);

        return photomaskStack;
    }
}
