package mrtjp.projectred.fabrication.item;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.integration.GateType;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

import javax.annotation.Nullable;
import java.util.List;

import static mrtjp.projectred.fabrication.editor.EditorDataUtils.*;

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

        if (!context.getPlayer().isCreative()) return InteractionResult.PASS;

        // Creative mode bypass: Convert blueprint directly to gate block
        BlockState blockState = context.getLevel().getBlockState(context.getClickedPos());
        if (blockState.getBlock() == FabricationReferences.IC_WORKBENCH_BLOCK) { return InteractionResult.PASS; }

        if (!canFabricate(stack.getTag())) {
            return InteractionResult.PASS;
        }

        ItemStack gate = GateType.FABRICATED_GATE.makeStack();
        gate.setTag(createFabricationCopy(stack.getTag()));

        context.getPlayer().addItem(gate);
        return InteractionResult.SUCCESS;
    }

    public static void buildTooltip(CompoundTag blueprintTag, List<Component> tooltipList) {

        if (blueprintTag == null) return;

        if (!hasFabricationTarget(blueprintTag)) {
            tooltipList.add(new TextComponent("<!> ").withStyle(ChatFormatting.RED)
                    .append(new TextComponent("Corrupted NBT data, please discard").withStyle(ChatFormatting.GRAY)));
            return;
        }

        //TODO localize
        tooltipList.add(new TextComponent("Name: " + blueprintTag.getString(KEY_IC_NAME)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("Tile count: " + blueprintTag.getInt(KEY_TILE_COUNT)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("IO Types: ").withStyle(ChatFormatting.GRAY));

        //TODO handle other types of IO
        byte bmask = blueprintTag.getByte(KEY_IO_BUNDLED);
        tooltipList.add(new TextComponent("  Top: " + getBundledIOString(bmask, 0)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("  Right: " + getBundledIOString(bmask, 1)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("  Bottom: " + getBundledIOString(bmask, 2)).withStyle(ChatFormatting.GRAY));
        tooltipList.add(new TextComponent("  Left: " + getBundledIOString(bmask, 3)).withStyle(ChatFormatting.GRAY));

        //TODO errors
    }

    private static String getBundledIOString(byte bmask, int r) {
        int i = 0x01 << r;
        int o = 0x10 << r;
        return ((bmask & i) != 0 ? "Bundled input" : (bmask & o) != 0 ? "Bundled output" : "None");
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
