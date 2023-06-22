package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.expansion.CraftingHelper;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import mrtjp.projectred.expansion.inventory.container.AutoCrafterContainer;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.world.World;

public class AutoCrafterTile extends BaseMachineTile implements CraftingHelper.InventorySource {

    private static final int KEY_CYCLE_PLAN = 2;

    private final Inventory planInventory = new Inventory(9) {
        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return RecipePlanItem.hasRecipeInside(stack);
        }
    };
    private final Inventory storageInventory = new Inventory(18);
    private final Inventory craftingGrid = new Inventory(9);

    private final CraftingHelper craftingHelper = new CraftingHelper(this);

    private boolean recipeNeedsUpdate = true;
    private int planSlot = 0;
    private int idleTicksOnPlan = 0;

    public AutoCrafterTile() {
        super(ExpansionReferences.AUTO_CRAFTER_TILE);
        planInventory.addListener(this::onInventoryChanged);
        storageInventory.addListener(this::onInventoryChanged);
    }

    @Override
    public void saveToNBT(CompoundNBT tag) {
        super.saveToNBT(tag);
        tag.put("storage_inv", storageInventory.createTag());
        tag.put("plan_inv", planInventory.createTag());
        tag.putByte("plan_slot", (byte) planSlot);
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
        super.loadFromNBT(tag);
        storageInventory.fromTag(tag.getList("storage_inv", 10));
        planInventory.fromTag(tag.getList("plan_inv", 10));
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
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayerEntity player) {
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
    protected Container createMenu(int windowId, PlayerInventory playerInventory, PlayerEntity player) {
        return new AutoCrafterContainer(playerInventory, this, windowId);
    }

    @Override
    public void onBlockRemoved() {
        super.onBlockRemoved();
        Vector3 pos = Vector3.fromTileCenter(this);
        dropInventory(planInventory, getLevel(), pos);
        dropInventory(storageInventory, getLevel(), pos);
    }

    private void onInventoryChanged(IInventory inventory) {
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
    public IInventory getCraftingMatrix() {
        return craftingGrid;
    }

    @Override
    public IInventory getStorage() {
        return storageInventory;
    }

    @Override
    public World getWorld() {
        return getLevel();
    }
    //endregion

    //region Container getters
    public Inventory getPlanInventory() {
        return planInventory;
    }

    public Inventory getStorageInventory() {
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
}
