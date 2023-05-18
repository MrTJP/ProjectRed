package mrtjp.projectred.exploration.item;

import codechicken.lib.util.ServerUtils;
import mrtjp.projectred.exploration.ProjectRedExploration;
import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.exploration.init.ExplorationTags;
import mrtjp.projectred.exploration.inventory.container.BackpackContainer;
import net.minecraft.client.util.ITooltipFlag;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.DyeColor;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ActionResult;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;

import javax.annotation.Nullable;
import java.util.List;

public class BackpackItem extends Item {

    public static final String TAG_INVENTORY = "backpack_inventory";

    private final int colour;

    public BackpackItem(int colour) {
        super(new Item.Properties()
                .stacksTo(1)
                .tab(ProjectRedExploration.EXPLORATION_GROUP));

        this.colour = colour;
    }

    @Override
    public ActionResultType useOn(ItemUseContext context) {

        if (!context.getLevel().isClientSide) {
            ServerPlayerEntity player = (ServerPlayerEntity) context.getPlayer();
            openGui(player);
        }
        return ActionResultType.sidedSuccess(context.getLevel().isClientSide);
    }

    @Override
    public ActionResult<ItemStack> use(World world, PlayerEntity player, Hand hand) {
        if (!world.isClientSide) {
            ServerPlayerEntity serverPlayer = (ServerPlayerEntity) player;
            openGui(serverPlayer);
        }
        return ActionResult.success(player.getItemInHand(hand));
    }

    private void openGui(ServerPlayerEntity player) {
        ServerUtils.openContainer(player,
                new SimpleNamedContainerProvider((windowId, playerInventory, playerEntity) -> new BackpackContainer(windowId, playerInventory),
                        new TranslationTextComponent(this.getDescriptionId())));
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable World world, List<ITextComponent> tooltip, ITooltipFlag flag) {
        int itemCount = getBackpackItemCount(stack);
        tooltip.add(new StringTextComponent(itemCount + " / 27").withStyle(TextFormatting.GRAY));
    }

    public DyeColor getDyeColor() {
        return DyeColor.byId(colour);
    }

    public static boolean isBackpack(ItemStack stack) {
        return stack.getItem() instanceof BackpackItem;
    }

    public static boolean hasBackpackInventory(ItemStack stack) {
        return isBackpack(stack) && stack.hasTag() && stack.getTag().contains(TAG_INVENTORY);
    }

    public static CompoundNBT getBackpackInventoryTag(ItemStack stack) {
        if (hasBackpackInventory(stack)) {
            return stack.getTag().getCompound(TAG_INVENTORY);
        }
        return new CompoundNBT();
    }

    public static int getBackpackItemCount(ItemStack stack) {
        if (hasBackpackInventory(stack)) {
            return BaseInventory.getItemCount(stack.getTag().getCompound(TAG_INVENTORY));
        }
        return 0;
    }

    public static void saveBackpackInventory(ItemStack stack, CompoundNBT inventoryTag) {
        stack.getOrCreateTag().put(TAG_INVENTORY, inventoryTag);
    }

    public static void deleteBackpackInventory(ItemStack stack) {
        if (hasBackpackInventory(stack)) {
            stack.getTag().remove(TAG_INVENTORY);
        }
    }

    public static boolean isItemAllowedInBackpack(ItemStack stack) {
        return !ExplorationTags.BACKPACKS_DISALLOWED_TAG.contains(stack.getItem());
    }
}
