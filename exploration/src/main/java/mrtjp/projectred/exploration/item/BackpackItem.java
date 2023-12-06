package mrtjp.projectred.exploration.item;

import codechicken.lib.util.ServerUtils;
import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.exploration.ProjectRedExploration;
import mrtjp.projectred.exploration.init.ExplorationTags;
import mrtjp.projectred.exploration.inventory.container.BackpackContainer;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;

public class BackpackItem extends Item {

    public static final String TAG_INVENTORY = "backpack_inventory";
    public static final String TAG_IS_OPENED = "is_opened";

    private final int colour;

    public BackpackItem(int colour) {
        super(new Item.Properties()
                .stacksTo(1)
                .tab(ProjectRedExploration.EXPLORATION_CREATIVE_TAB));

        this.colour = colour;
    }

    @Override
    public InteractionResult useOn(UseOnContext context) {

        if (!context.getLevel().isClientSide) {
            ServerPlayer player = (ServerPlayer) context.getPlayer();
            if (player != null) {
                openGui(player);
            }
        }
        return InteractionResult.sidedSuccess(context.getLevel().isClientSide);
    }

    @Override
    public InteractionResultHolder<ItemStack> use(Level world, Player player, InteractionHand hand) {
        if (!world.isClientSide) {
            ServerPlayer serverPlayer = (ServerPlayer) player;
            openGui(serverPlayer);
        }
        return InteractionResultHolder.success(player.getItemInHand(hand));
    }

    private void openGui(ServerPlayer player) {
        ServerUtils.openContainer(player,
                new SimpleMenuProvider((windowId, playerInventory, playerEntity) -> new BackpackContainer(windowId, playerInventory),
                        new TranslatableComponent(this.getDescriptionId())));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable Level world, List<Component> tooltip, TooltipFlag flag) {
        if (isBackpackOpened(stack)) return;

        int itemCount = getBackpackItemCount(stack);
        tooltip.add(new TextComponent(itemCount + " / 27").withStyle(ChatFormatting.GRAY));
    }

    public DyeColor getDyeColor() {
        return DyeColor.byId(colour);
    }

    public static boolean isBackpack(ItemStack stack) {
        return stack.getItem() instanceof BackpackItem;
    }

    public static boolean hasBackpackInventory(ItemStack stack) {
        return isBackpack(stack) && stack.hasTag() && Objects.requireNonNull(stack.getTag()).contains(TAG_INVENTORY);
    }

    public static CompoundTag getBackpackInventoryTag(ItemStack stack) {
        if (hasBackpackInventory(stack)) {
            return Objects.requireNonNull(stack.getTag()).getCompound(TAG_INVENTORY);
        }
        return new CompoundTag();
    }

    public static int getBackpackItemCount(ItemStack stack) {
        if (hasBackpackInventory(stack)) {
            return BaseInventory.getItemCount(Objects.requireNonNull(stack.getTag()).getCompound(TAG_INVENTORY));
        }
        return 0;
    }

    public static void saveBackpackInventory(ItemStack stack, CompoundTag inventoryTag) {
        stack.getOrCreateTag().put(TAG_INVENTORY, inventoryTag);
    }

    public static void deleteBackpackInventory(ItemStack stack) {
        if (hasBackpackInventory(stack)) {
            Objects.requireNonNull(stack.getTag()).remove(TAG_INVENTORY);
        }
    }

    public static void setBackpackOpenedFlag(ItemStack stack, boolean opened) {
        if (isBackpack(stack)) {
            CompoundTag tag = stack.getOrCreateTag();
            if (opened) {
                tag.putBoolean(TAG_IS_OPENED, true);
            } else {
                tag.remove(TAG_IS_OPENED);
            }
        }
    }

    public static boolean isBackpackOpened(ItemStack stack) {
        return isBackpack(stack) && stack.hasTag() && Objects.requireNonNull(stack.getTag()).getBoolean(TAG_IS_OPENED);
    }

    public static boolean isItemAllowedInBackpack(ItemStack stack) {
        return !stack.is(ExplorationTags.BACKPACKS_DISALLOWED_TAG);
    }
}
