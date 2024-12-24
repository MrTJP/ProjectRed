package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.inventory.BaseContainer;
import mrtjp.projectred.expansion.CraftingHelper;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.inventory.container.AutoCrafterMenu;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.wrapper.InvWrapper;

public class AutoCrafterBlockEntity extends BaseMachineBlockEntity implements CraftingHelper.InventorySource {

    private static final int KEY_CYCLE_PLAN = 2;

    private final BaseContainer planInventory = new BaseContainer(9) {
        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return RecipePlanItem.hasRecipeInside(stack);
        }
    };
    private final BaseContainer storageInventory = new BaseContainer(18);
    private final BaseContainer craftingGrid = new BaseContainer(9);

    private final IItemHandler handler = new InvWrapper(storageInventory);

    private final CraftingHelper craftingHelper = new CraftingHelper(this);

    private boolean recipeNeedsUpdate = true;
    private int planSlot = 0;
    private int idleTicksOnPlan = 0;

    public AutoCrafterBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.AUTO_CRAFTER_BLOCK_ENTITY.get(), pos, state);
        planInventory.addListener(this::onInventoryChanged);
        storageInventory.addListener(this::onInventoryChanged);
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        super.saveToNBT(tag);
        storageInventory.saveTo(tag, "storage_inv");
        planInventory.saveTo(tag, "plan_inv");
        tag.putByte("plan_slot", (byte) planSlot);
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        super.loadFromNBT(tag);
        storageInventory.loadFrom(tag, "storage_inv");
        planInventory.loadFrom(tag, "plan_inv");
        planSlot = tag.getByte("plan_slot") & 0xFF;
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        super.writeDesc(out);
    }

    @Override
    public void readDesc(MCDataInput in) {
        super.readDesc(in);
    }

    @Override
    public void receiveUpdateFromServer(int key, MCDataInput input) {
        super.receiveUpdateFromServer(key, input);
    }

    @Override
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayer player) {
        switch (key) {
            case KEY_CYCLE_PLAN:
                cyclePlan();
                break;
            default:
                super.receiveUpdateFromClient(key, input, player);
        }
    }

    public void sendCyclePlan() {
        sendUpdateToServer(KEY_CYCLE_PLAN, p -> {});
    }

    @Override
    protected AbstractContainerMenu createMenu(int windowId, Inventory playerInventory, Player player) {
        return new AutoCrafterMenu(playerInventory, this, windowId);
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        Vector3 pos = Vector3.fromTileCenter(this);
        dropInventory(planInventory, getLevel(), pos);
        dropInventory(storageInventory, getLevel(), pos);
    }

    private void onInventoryChanged(Container inventory) {
        recipeNeedsUpdate = true;
        setChanged();
    }

    @Override
    public void tick() {
        super.tick();
        if (getLevel().isClientSide) return;

        // Cycle plans if we are waiting too long for ingredients
        if (idleTicksOnPlan > getMaxPlanIdleTicks()) {
            cyclePlan();
        }

        updateRecipeIfNeeded();
    }

    public void updateRecipeIfNeeded() {
        if (recipeNeedsUpdate) {
            recipeNeedsUpdate = false;

            ItemStack plan = planInventory.getItem(planSlot);
            if (RecipePlanItem.hasRecipeInside(plan)) {
                RecipePlanItem.loadPlanInputsToGrid(craftingGrid, plan);
            } else {
                craftingGrid.clearContent();
            }

            craftingHelper.onInventoryChanged();
        }
    }

    private void cyclePlan() {
        int start = planSlot;
        do {
            planSlot = (planSlot + 1) % 9;
        } while (planSlot != start && planInventory.getItem(planSlot).isEmpty());

        if (planSlot != start) {
            recipeNeedsUpdate = true;
            idleTicksOnPlan = 0;
        }
    }

    private int getMaxPlanIdleTicks() {
        return 10;
    }

    //region CraftingHelper.InventorySource
    @Override
    public Container getCraftingMatrix() {
        return craftingGrid;
    }

    @Override
    public Container getStorage() {
        return storageInventory;
    }

    @Override
    public Level getWorld() {
        return getLevel();
    }
    //endregion

    //region Container getters
    public SimpleContainer getPlanInventory() {
        return planInventory;
    }

    public SimpleContainer getStorageInventory() {
        return storageInventory;
    }

    public int getPlanSlot() {
        return planSlot;
    }
    //endregion

    //region Machine Tile
    @Override
    protected boolean canStartOrContinueWork() {
        updateRecipeIfNeeded();
        boolean canTake = craftingHelper.canTakeIntoStorage();
        if (!canTake) {
            // Plans will be force-cycled if no work is done for a while
            idleTicksOnPlan++;
        }
        return canTake;
    }

    @Override
    protected int startWork() {
        return 20 * 5;
    }

    @Override
    protected int tickWork(int remainingWork) {
        updateRecipeIfNeeded();
        if (canConductorWork() && craftingHelper.canTakeIntoStorage()) {
            conductor.applyPower(-1100);
            return 1;
        }
        // Pause work if no charge or no space for results
        return 0;
    }

    @Override
    protected void finishWork() {
        updateRecipeIfNeeded();
        craftingHelper.onCraftedIntoStorage();
        cyclePlan();
    }
    //endregion

    //region Capabilities
    public IItemHandler getHandler() {
        return handler;
    }
    //endregion
}
