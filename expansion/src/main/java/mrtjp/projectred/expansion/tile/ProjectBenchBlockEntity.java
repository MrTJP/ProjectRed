package mrtjp.projectred.expansion.tile;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.util.ServerUtils;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.inventory.BaseContainer;
import mrtjp.projectred.core.tile.IPacketReceiverBlockEntity;
import mrtjp.projectred.core.tile.ProjectRedBlockEntity;
import mrtjp.projectred.expansion.CraftingHelper;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.inventory.container.ProjectBenchMenu;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.lib.InventoryLib;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.*;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.wrapper.InvWrapper;

public class ProjectBenchBlockEntity extends ProjectRedBlockEntity implements IPacketReceiverBlockEntity, CraftingHelper.InventorySource {

    private static final int KEY_WRITE_PLAN = 2;
    private static final int KEY_CLEAR_GRID = 3;

    private final BaseContainer planInventory = new BaseContainer(1) {
        @Override
        public boolean canPlaceItem(int slot, ItemStack stack) {
            return stack.getItem() instanceof RecipePlanItem;
        }
    };
    private final BaseContainer craftingGrid = new BaseContainer(9);
    private final BaseContainer storageInventory = new BaseContainer(18);
    private final BaseContainer planCraftingGrid = new BaseContainer(9);

    private final CraftingHelper craftingHelper = new CraftingHelper(this);
    private final IItemHandler handler = new InvWrapper(storageInventory);

    private boolean isPlanRecipe = false;
    private boolean recipeNeedsUpdate = true;

    public ProjectBenchBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.PROJECT_BENCH_BLOCK_ENTITY.get(), pos, state);

        planInventory.addListener(this::onInventoryChanged);
        craftingGrid.addListener(this::onInventoryChanged);
        storageInventory.addListener(this::onInventoryChanged);
        // not needed for planCraftingGrid
    }

    @Override
    public void saveToNBT(CompoundTag tag) {
        planInventory.saveTo(tag, "plan_inv");
        craftingGrid.saveTo(tag, "crafting_inv");
        storageInventory.saveTo(tag, "storage_inv");
        planCraftingGrid.saveTo(tag, "plan_crafting_inv");
    }

    @Override
    public void loadFromNBT(CompoundTag tag) {
        planInventory.loadFrom(tag, "plan_inv");
        craftingGrid.loadFrom(tag, "crafting_inv");
        storageInventory.loadFrom(tag, "storage_inv");
        planCraftingGrid.loadFrom(tag, "plan_crafting_inv");
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
    public void receiveUpdateFromClient(int key, MCDataInput input, ServerPlayer player) {
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
    public InteractionResult onBlockActivated(Player player, InteractionHand hand, BlockHitResult hit) {
        if (!getLevel().isClientSide) {
            ServerUtils.openContainer(
                    (ServerPlayer) player,
                    new SimpleMenuProvider(
                            (id, inv, p) -> new ProjectBenchMenu(inv, this, id),
                            getBlockState().getBlock().getName()),
                    p -> p.writePos(getBlockPos()));
        }

        return InteractionResult.sidedSuccess(getLevel().isClientSide);
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

    private void onInventoryChanged(Container inventory) {
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

    private void clearGrid(ServerPlayer player) {
        // Target this player's container specifically in case clearing grid will require
        // items to spill into player inventory
        AbstractContainerMenu container = player.containerMenu;
        if (container instanceof ProjectBenchMenu) {
            ((ProjectBenchMenu) container).transferAllFromGrid();
            updateRecipeIfNeeded();
        }
    }

    //region CraftingHelper.InventorySource
    @Override
    public Container getCraftingMatrix() {
        return isPlanRecipe ? planCraftingGrid : craftingGrid;
    }

    @Override
    public Container getStorage() {
        return storageInventory;
    }

    @Override
    public boolean canConsumeFromCraftingMatrix() {
        return !isPlanRecipe;
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

    public SimpleContainer getCraftingGridInventory() {
        return craftingGrid;
    }

    public SimpleContainer getStorageInventory() {
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
    public IItemHandler getHandler() {
        return handler;
    }
    //endregion
}
