package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.tile.IPacketReceiverTile;
import mrtjp.projectred.core.tile.ProjectRedTile;
import mrtjp.projectred.expansion.CraftingHelper;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import mrtjp.projectred.expansion.inventory.container.ProjectBenchContainer;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.wrapper.InvWrapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class ProjectBenchTile extends ProjectRedTile implements ITickableTileEntity, IPacketReceiverTile, CraftingHelper.InventorySource {

    private static final int KEY_WRITE_PLAN = 2;
    private static final int KEY_CLEAR_GRID = 3;

    private final Inventory planInventory = new Inventory(1) {
        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return stack.getItem() instanceof RecipePlanItem;
        }
    };
    private final Inventory craftingGrid = new Inventory(9);
    private final Inventory storageInventory = new Inventory(18);
    private final Inventory planCraftingGrid = new Inventory(9);

    private final CraftingHelper craftingHelper = new CraftingHelper(this);
    private final LazyOptional<?> storageInventoryCap = LazyOptional.of(this::createStorageInventoryCap);

    private boolean isPlanRecipe = false;
    private boolean recipeNeedsUpdate = true;

    public ProjectBenchTile() {
        super(ExpansionReferences.PROJECT_BENCH_TILE);

        planInventory.addListener(this::onInventoryChanged);
        craftingGrid.addListener(this::onInventoryChanged);
        storageInventory.addListener(this::onInventoryChanged);
        // not needed for planCraftingGrid
    }

    @Override
    public void saveToNBT(CompoundNBT tag) {
        tag.put("plan_inv", planInventory.createTag());
        tag.put("crafting_inv", craftingGrid.createTag());
        tag.put("storage_inv", storageInventory.createTag());
        tag.put("plan_crafting_inv", planCraftingGrid.createTag());
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
        planInventory.fromTag(tag.getList("plan_inv", 10));
        craftingGrid.fromTag(tag.getList("crafting_inv", 10));
        storageInventory.fromTag(tag.getList("storage_inv", 10));
        planCraftingGrid.fromTag(tag.getList("plan_crafting_inv", 10));
    }

    @Override
    public void writeDesc(MCDataOutput out) {
    }

    @Override
    public void readDesc(MCDataInput in) {
    }

    @Override
    public void receiveUpdateFromServer(int key, MCDataInput input) {
    }

    @Override
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayerEntity player) {
        switch (key) {
            case KEY_WRITE_PLAN:
                writePlan();
                break;
            case KEY_CLEAR_GRID:
                clearGrid(player);
                break;
            default:
                //Error
        }
    }

    public void sendWriteButtonPressed() {
        sendUpdateToServer(KEY_WRITE_PLAN, out -> {});
    }

    public void sendGridClearButtonPressed() {
        sendUpdateToServer(KEY_CLEAR_GRID, out -> {});
    }

    @Override
    public ActionResultType onBlockActivated(PlayerEntity player, Hand hand, BlockRayTraceResult hit) {
        if (!getLevel().isClientSide) {
            ServerUtils.openContainer(
                    (ServerPlayerEntity) player,
                    new SimpleNamedContainerProvider(
                            (id, inv, p) -> new ProjectBenchContainer(inv, this, id),
                            new TranslationTextComponent(getBlockState().getBlock().getDescriptionId())),
                    p -> p.writePos(getBlockPos()));
        }

        return ActionResultType.sidedSuccess(getLevel().isClientSide);
    }

    @Override
    public void onBlockRemoved() {
        Vector3 pos = Vector3.fromTileCenter(this);
        dropInventory(craftingGrid, getLevel(), pos);
        dropInventory(planInventory, getLevel(), pos);
        dropInventory(storageInventory, getLevel(), pos);
    }

    @Override
    public void tick() {
        updateRecipeIfNeeded();

        if (!getLevel().isClientSide) {
            transferExcessToStorage();
        }
    }

    public void updateRecipeIfNeeded() {
        if (recipeNeedsUpdate) {
            recipeNeedsUpdate = false;

            // Check if recipe should be sourced from plan
            isPlanRecipe = false;

            // Grid must be empty
            boolean gridEmpty = true;
            for (int i = 0; i < 9; i++) {
                if (!craftingGrid.getItem(i).isEmpty()) {
                    gridEmpty = false;
                    break;
                }
            }

            // And plan must be present with recipe
            if (gridEmpty) {
                ItemStack plan = planInventory.getItem(0);
                if (RecipePlanItem.hasRecipeInside(plan)) {
                    isPlanRecipe = true; // This flag changes what inventory is presented to crafting helper
                    RecipePlanItem.loadPlanInputsToGrid(planCraftingGrid, plan);
                }
            }

            craftingHelper.onInventoryChanged();
        }
    }

    private void onInventoryChanged(IInventory inventory) {
        recipeNeedsUpdate = true;
        setChanged();
    }

    private void transferExcessToStorage() {
        for (int i = 0; i < 9; i++) {
            ItemStack stack = craftingGrid.getItem(i);
            if (!stack.isEmpty() && stack.getCount() > 1) {
                int toMove = Math.max(1, stack.getCount() / 4);
                ItemStack removed = craftingGrid.removeItem(i, toMove);
                InventoryLib.injectItemStack(storageInventory, removed, false);
                if (!removed.isEmpty()) {
                    craftingGrid.getItem(i).grow(removed.getCount());
                    craftingGrid.setChanged();
                }
            }
        }
    }

    private void writePlan() {
        updateRecipeIfNeeded();

        if (craftingHelper.hasRecipe() && !isPlanRecipe) {
            ItemStack planStack = planInventory.getItem(0);
            ItemStack result = craftingHelper.getRecipeOutout();

            if (!planStack.isEmpty() && !result.isEmpty()) {
                ItemStack[] inputs = new ItemStack[9];
                for (int i = 0; i < 9; i++) {
                    inputs[i] = craftingGrid.getItem(i).copy();
                }
                RecipePlanItem.savePlan(planStack, inputs, result);
            }
        }
    }

    private void clearGrid(ServerPlayerEntity player) {
        // Target this player's container specifically in case clearing grid will require
        // items to spill into player inventory
        Container container = player.containerMenu;
        if (container instanceof ProjectBenchContainer) {
            ((ProjectBenchContainer) container).transferAllFromGrid();
            updateRecipeIfNeeded();
        }
    }

    //region CraftingHelper.InventorySource
    @Override
    public IInventory getCraftingMatrix() {
        return isPlanRecipe ? planCraftingGrid : craftingGrid;
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

    public Inventory getCraftingGridInventory() {
        return craftingGrid;
    }

    public Inventory getStorageInventory() {
        return storageInventory;
    }

    public boolean isPlanRecipe() {
        return isPlanRecipe;
    }

    public CraftingHelper getCraftingHelper() {
        return craftingHelper;
    }
    //endregion

    //region Capabilities
    @Nonnull
    @Override
    public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
        if (!this.remove && cap == net.minecraftforge.items.CapabilityItemHandler.ITEM_HANDLER_CAPABILITY)
            return storageInventoryCap.cast();
        return super.getCapability(cap, side);
    }

    @Override
    protected void invalidateCaps() {
        super.invalidateCaps();
        storageInventoryCap.invalidate();
    }

    private IItemHandler createStorageInventoryCap() {
        return new InvWrapper(storageInventory);
    }
    //endregion
}
