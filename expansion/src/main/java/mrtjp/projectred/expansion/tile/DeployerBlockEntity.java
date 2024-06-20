package mrtjp.projectred.expansion.tile;

import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import com.mojang.authlib.GameProfile;
import mrtjp.projectred.core.inventory.BaseInventory;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.inventory.container.DeployerContainerMenu;
import net.minecraft.commands.arguments.EntityAnchorArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ForgeCapabilities;
import net.minecraftforge.common.util.FakePlayerFactory;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.event.entity.player.PlayerInteractEvent.RightClickBlock;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.wrapper.InvWrapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

import static mrtjp.projectred.expansion.ProjectRedExpansion.LOGGER;

public class DeployerBlockEntity extends BaseDeviceTile {

    // Fake player ID used to break blocks
    private static final GameProfile PR_FAKE_PLAYER = new GameProfile(UUID.fromString("6140461b-e5b4-41ba-beb1-dce616e6abc0"), "[ProjectRed]");

    private final BaseInventory inventory = new BaseInventory(9);

    private final LazyOptional<? extends IItemHandler> handler = LazyOptional.of(() -> new InvWrapper(inventory));

    public DeployerBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.DEPLOYER_TILE.get(), pos, state);
        inventory.addListener(c -> setChanged());
    }

    //region Save/load
    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        inventory.saveTo(tag, "inventory");
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        inventory.loadFrom(tag, "inventory");
    }
    //endregion

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        dropInventory(inventory, getLevel(), Vector3.fromBlockPos(getBlockPos()));
    }

    @Override
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        if (!getLevel().isClientSide) {
            ServerUtils.openContainer(
                    (ServerPlayer) player,
                    new SimpleMenuProvider(
                            (id, inv, p) -> new DeployerContainerMenu(inv, this, id),
                            getBlockState().getBlock().getName()),
                    p -> p.writePos(getBlockPos()));
        }
        return InteractionResult.sidedSuccess(getLevel().isClientSide);
    }

    @Override
    protected void onActivated() {
        // Prepare fake player
        Player player = FakePlayerFactory.get((ServerLevel) getLevel(), PR_FAKE_PLAYER);

        // Load items into player
        player.getInventory().clearContent();
        for (int i = 0; i < inventory.getContainerSize(); i++) {
            player.getInventory().setItem(i, inventory.getItem(i));
        }

        // Try to use one of the items from the player
        boolean success = deployFromFakePlayer(player);

        // Save items back to inventory if anything happened
        if (success) {
            // Copy back first n items
            for (int i = 0; i < inventory.getContainerSize(); i++) {
                inventory.setItem(i, player.getInventory().getItem(i));
            }
            // Copy back any new items that may have been added
            List<ItemStack> remaining = new LinkedList<>();
            for (int i = inventory.getContainerSize(); i < player.getInventory().getContainerSize(); i++) {
                ItemStack rem = inventory.addItem(player.getInventory().getItem(i));
                if (!rem.isEmpty()) {
                    remaining.add(rem);
                }
            }
            // Do something with excess
            if (!remaining.isEmpty())
                handlePostDeployExcess(remaining);
        }
    }

    @Override
    protected void onDeactivated() {
    }

    protected boolean deployFromFakePlayer(Player player) {
        // Create a hit for fake player
        Direction useDir = Direction.values()[side ^ 1];
        BlockPos usePos = getBlockPos().relative(useDir);
        Vec3 hitTarget = new Vec3(usePos.getX() + 0.5, usePos.getY() + 0.5, usePos.getZ() + 0.5);
        BlockHitResult hit = new BlockHitResult(hitTarget, Direction.values()[side], usePos, false);

        // Locate fake player at deployer block (Things like bows use players location to spawn arrows)
        player.setPos(
                getBlockPos().getX() + 0.5 + (useDir.getStepX() * 0.9),
                getBlockPos().getY() + 0.5 + (useDir.getStepY() * 0.9) - player.getEyeHeight(),
                getBlockPos().getZ() + 0.5 + (useDir.getStepZ() * 0.9));
        player.lookAt(EntityAnchorArgument.Anchor.EYES, hitTarget);

        // Try to use one of the items from player inventory
        for (int i = 0; i < player.getInventory().getContainerSize(); i++) {
            ItemStack stack = player.getInventory().getItem(i);
            if (stack.isEmpty()) continue;

            // Swap player's main-hand with item
            ItemStack mainHand = player.getMainHandItem();
            player.setItemSlot(EquipmentSlot.MAINHAND, stack);
            player.getInventory().setItem(i, mainHand);

            // Try to use it
            InteractionResult result = runInteractWithEntityLogic(player, usePos, hit);
            if (!result.consumesAction()) {
                result = runUseOnLogic(player, usePos, hit);
            }

            // Put item from main-hand back into inventory
            ItemStack slotStack = player.getInventory().getItem(i);
            player.getInventory().setItem(i, player.getMainHandItem());
            player.setItemSlot(EquipmentSlot.MAINHAND, slotStack);

            // Return if success
            if (result.consumesAction()) {
                return true;
            }
        }

        return false;
    }

    // Mimics the logic of and MultiPlayerGameMode#performUseItemOn
    protected InteractionResult runUseOnLogic(Player player, BlockPos usePos, BlockHitResult hit) {
        RightClickBlock event = ForgeHooks.onRightClickBlock(player, InteractionHand.MAIN_HAND, usePos, hit);
        if (event.isCanceled()) {
            return event.getCancellationResult();
        }

        // Item gets the first use call
        UseOnContext ctx = new UseOnContext(player, InteractionHand.MAIN_HAND, hit);
        if (event.getUseItem() != Event.Result.DENY) {
            InteractionResult result = player.getMainHandItem().onItemUseFirst(ctx);
            if (result.consumesAction()) {
                return result;
            }
        }

        // Block gets the second use call. It can optionally consume the right-click
        if (event.getUseBlock() != Event.Result.DENY) {
            InteractionResult result = getLevel().getBlockState(usePos).use(getLevel(), player, InteractionHand.MAIN_HAND, hit);
            if (result.consumesAction()) {
                return result;
            }
        }

        // If event cancelled the item use, don't run the remaining item use calls
        if (event.getUseItem() == Event.Result.DENY) {
            return InteractionResult.PASS;
        }

        // Item again gets a post-block use call. Here it will try to again use itself on the target block
        InteractionResult result = player.getMainHandItem().useOn(ctx);
        if (result.consumesAction()) {
            return result;
        }

        // At this point, block use has failed. PLayer use is now attempted (i.e. food, weapons, etc)
        result = ForgeHooks.onItemRightClick(player, InteractionHand.MAIN_HAND);
        if (result != null) {
            return result;
        }

        // Try to use the held item directly
        player.useItem = ItemStack.EMPTY;
        player.useItemRemaining = 0;
        InteractionResultHolder<ItemStack> resultHolder = player.getMainHandItem().use(getLevel(), player, InteractionHand.MAIN_HAND);
        if (resultHolder.getResult().consumesAction()) {
            player.setItemInHand(InteractionHand.MAIN_HAND, resultHolder.getObject());
            // If this is a hold-able item like a bow, release it
            if (!player.useItem.isEmpty()) {
                player.useItemRemaining = 0; // Fast-forward to max use time
                player.releaseUsingItem();
            }
            return resultHolder.getResult();
        }

        return InteractionResult.PASS;
    }

    // Mimics the logic of MultiPlayerGameMode#interact and MultiPlayerGameMode#interactAt
    protected InteractionResult runInteractWithEntityLogic(Player player, BlockPos usePos, BlockHitResult hit) {
        List<Entity> entities = getLevel().getEntities(player, new AABB(usePos)); // Get entities excluding player

        for (var entity : entities) {
            // First, try player interact
            InteractionResult result = player.interactOn(entity, InteractionHand.MAIN_HAND);
            if (result.consumesAction()) {
                return result;
            }

            // Then try direct interaction
            Vec3 vec3 = hit.getLocation().subtract(entity.getX(), entity.getY(), entity.getZ());
            result = ForgeHooks.onInteractEntityAt(player, entity, vec3, InteractionHand.MAIN_HAND);
            if (result != null) {
                return result;
            }
            result = entity.interactAt(player, vec3, InteractionHand.MAIN_HAND);
            if (result.consumesAction()) {
                return result;
            }
        }

        return InteractionResult.PASS;
    }

    protected void handlePostDeployExcess(List<ItemStack> excess) {
        LOGGER.warn("Deployer at {} has excess items: {}", getBlockPos(), excess);
    }

    //region Capabilities
    @Nonnull
    @Override
    public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
        if (!this.remove && cap == ForgeCapabilities.ITEM_HANDLER) {
            if (side != Direction.values()[this.side ^ 1]) {
                return handler.cast();
            }
        }
        return super.getCapability(cap, side);
    }

    @Override
    public void invalidateCaps() {
        super.invalidateCaps();
        handler.invalidate();
    }
    //endregion

    //region ContainerMenu interface
    public BaseInventory getInventory() {
        return inventory;
    }
    //endregion
}
