package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import mrtjp.projectred.core.GuiRestrictedSlot.ISlotCheck;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemHoe;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemSword;
import net.minecraft.item.ItemTool;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.world.chunk.Chunk;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.common.registry.GameRegistry;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class TileAlloySmelter extends TileBasicsBase implements IInventory {

    private SimpleInventory _inv = new SimpleInventory(11, "alloy", 64);

    boolean updateNextTick;
    
    // True when working on something
    public boolean hasWork = false;

    // Between 0 and whatever the recipe requires
    public int progress = 0;

    // Between 0 and 6400
    public int heat = 0;

    // Next tick, all near by will be notified.
    public boolean queueWatcherUpdate = false;
    // Next tick, work will be rechecked.
    public boolean queueWorkUpdate = false;
    // Cache, used to avoid checking all recipes every tick.
    public int burnTimeForRecipe = 0;
    
    public TileAlloySmelter() {}

    @Override
    public void onBlockBreak() {
        BasicUtils.dropItem(worldObj, xCoord, yCoord, zCoord, new ItemStack(ProjectRedCore.blockMachines.blockID, 1, EnumBasics.ALLOYSMELTER.meta));
        _inv.dropContents(worldObj, xCoord, yCoord, zCoord);
    }

    public Container getContainer(EntityPlayer player) {
        GhostContainer ghost = new GhostContainer(player.inventory, this);
        // Crafting matrix
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                final int slotNumber = i * 3 + j;
                ghost.addNormalSlot(slotNumber, this, 44 + (j * 18), 17 + (i * 18));
            }
        }
        // Fuel slot
        ghost.addRestrictedSlot(9, this, 107, 17, new ISlotCheck() {
            @Override
            public boolean isSlotAllowed(ItemStack stack) {
                return getItemBurnTime(stack) > 0;
            }
        });
        // Result slot
        ghost.addRestrictedSlot(10, this, 141, 47, -1);
        // Player inv slots
        ghost.addNormalSlotsForPlayerInventory(8, 84);
        return ghost;
    }

    @Override
    public void onBlockClicked(EntityPlayer player) {

    }

    @Override
    public boolean onBlockActivated(EntityPlayer player) {
        if (!player.isSneaking()) {
            player.openGui(ProjectRedCore.instance, CoreGuiHandler.alloyID, player.worldObj, xCoord, yCoord, zCoord);
            return true;
        }
        return false;
    }

    @Override
    public void writeToNBT(NBTTagCompound nbt) {
        super.writeToNBT(nbt);
        _inv.writeToNBT(nbt);
        nbt.setInteger("heat", heat);
        nbt.setInteger("progress", progress);
        nbt.setInteger("time", burnTimeForRecipe);
    }

    @Override
    public void readFromNBT(NBTTagCompound nbt) {
        super.readFromNBT(nbt);
        _inv.readFromNBT(nbt);
        heat = nbt.getInteger("heat");
        progress = nbt.getInteger("progress");
        burnTimeForRecipe = nbt.getInteger("time");
        updateNextTick = true;
    }

    @Override
    public Packet getDescriptionPacket() {
        NBTTagCompound nbt = new NBTTagCompound();
        writeToNBT(nbt);
        return new Packet132TileEntityData(this.xCoord, this.yCoord, this.zCoord, 3, nbt);
    }

    @Override
    public int getSizeInventory() {
        return _inv.getSizeInventory();
    }

    @Override
    public ItemStack getStackInSlot(int i) {
        return _inv.getStackInSlot(i);
    }

    @Override
    public ItemStack decrStackSize(int i, int j) {
        ItemStack stack = _inv.decrStackSize(i, j);
        return stack;
    }

    @Override
    public ItemStack getStackInSlotOnClosing(int i) {
        return _inv.getStackInSlotOnClosing(i);
    }

    @Override
    public void setInventorySlotContents(int i, ItemStack itemstack) {
        _inv.setInventorySlotContents(i, itemstack);
    }

    @Override
    public String getInvName() {
        return _inv.getInvName();
    }

    @Override
    public boolean isInvNameLocalized() {
        return true;
    }

    @Override
    public int getInventoryStackLimit() {
        return _inv.getInventoryStackLimit();
    }

    @Override
    public boolean isUseableByPlayer(EntityPlayer player) {
        return _inv.isUseableByPlayer(player);
    }

    @Override
    public void openChest() {
        _inv.openChest();
    }

    @Override
    public void closeChest() {
        _inv.closeChest();
    }

    @Override
    public boolean isItemValidForSlot(int i, ItemStack itemstack) {
        return true;
    }    

    @Override
    public int getLightLevel() {
        return heat > 0 ? (hasWork ? 13 : 10) : 0;
    }

    @Override
    public void updateEntity() {
        if (queueWorkUpdate) {
            queueWorkUpdate = false;
            hasWork = hasWork();
        }
        if (hasWork && heat <= 0) {
            eatFuel();
            if (heat > 0) {
                queueWatcherUpdate = true;
                updateNextTick = true;
            }
        }
        if (heat > 0) {
            if (BasicUtils.isClient(worldObj))
                spawnParticles();
            if (hasWork)
                progress++;
            if (heat > 0)
                heat--;
            if (heat <= 0)
                updateNextTick = true;
            queueWatcherUpdate = true;
        }
        if (hasWork)
            if (progress >= burnTimeForRecipe) {
                AlloySmelterRecipe r = getSuggestedRecipe();
                if (r != null) {
                    eatAllResourcesForRecipe(r);
                    ItemStack result = r.getResult();
                    r._handler.onItemCrafted(result);
                    if (_inv.getStackInSlot(10) == null)
                        _inv.setInventorySlotContents(10, result);
                    else {
                        ItemStack inslot = _inv.getStackInSlot(10);
                        inslot.stackSize += result.stackSize;
                        _inv.setInventorySlotContents(10, inslot);
                    }
                    progress = 0;
                    burnTimeForRecipe = 0;
                    queueWatcherUpdate = true;
                    queueWorkUpdate = true;
                    updateNextTick = true;
                }
            }
        if (queueWatcherUpdate) {

            queueWatcherUpdate = false;
            updateWatchers();
        }
        if (updateNextTick) {
            // All nearby players need to be updated if the status of work
            // changes, or if heat runs out / starts up, in order to change
            // texture.
            updateNextTick = false;
            worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
            worldObj.updateAllLightTypes(xCoord, yCoord, zCoord);
        }
    }

    @SideOnly(Side.CLIENT)
    private void spawnParticles() {

    }

    public void updateWatchers() {
        if (BasicUtils.isClient(worldObj))
            return;
        PacketCustom packet = new PacketCustom(CoreSPH.channel, CoreSPH.alloySmelterWatcherUpdate);
        packet.writeCoord(xCoord, yCoord, zCoord);
        packet.writeShort(heat);
        packet.writeShort(progress);
        Chunk c = worldObj.getChunkFromBlockCoords(xCoord, zCoord);
        packet.sendToChunk(worldObj, c.xPosition, c.zPosition);
    }

    public boolean hasWork() {
        AlloySmelterRecipe r =  getSuggestedRecipe();
        boolean shouldWork = (r != null && (_inv.getStackInSlot(10) == null || (BasicUtils.areStacksTheSame(_inv.getStackInSlot(10), r.getResult()) && _inv.getStackInSlot(10).stackSize + r.getResult().stackSize <= _inv.getInventoryStackLimit())));        
        if (shouldWork) {
            burnTimeForRecipe = r.getBurnTime();
            updateNextTick = true;
            return true;
        }
        progress = 0;
        burnTimeForRecipe = 0;
        return false;
    }

    private void eatFuel() {
        int burntime = getItemBurnTime(_inv.getStackInSlot(9));
        if (burntime > 0) {
            _inv.decrStackSize(9, 1);
            heat += burntime;
            if (heat > 6400)
                heat = 6400;
        }
    }

    private void eatResource(ItemStack s) {
        int missing = s.stackSize;
        while (missing > 0)
            for (int i = 0; i < 9; i++) {
                ItemStack stackinslot = _inv.getStackInSlot(i);
                if (stackinslot == null)
                    continue;
                if (BasicUtils.areStacksTheSame(stackinslot, s)) {
                    _inv.decrStackSize(i, 1);
                    missing--;
                    if (missing <= 0)
                        break;
                }
            }
    }

    private void eatAllResourcesForRecipe(AlloySmelterRecipe r) {
        for (ItemStack s : r.getMatrix())
            eatResource(s);
    }

    public AlloySmelterRecipe getSuggestedRecipe() {
        for (AlloySmelterRecipe r : AlloySmelterRecipe.getAlloyRecipes())
            if (r.calculateMatch(getCraftingMatrix()))
                return r;
        return null;
    }

    private ItemStack[] getCraftingMatrix() {
        ItemStack[] matrix = new ItemStack[9];
        for (int i = 0; i < 9; i++) {
            ItemStack inslot = _inv.getStackInSlot(i);
            if (inslot != null)
                matrix[i] = inslot.copy();
            else
                matrix[i] = null;
        }
        return matrix;
    }

    private int getItemBurnTime(ItemStack stack) {
        if (stack == null)
            return 0;
        else {
            int i = stack.getItem().itemID;
            Item item = stack.getItem();
            if (stack.getItem() instanceof ItemBlock && Block.blocksList[i] != null) {
                Block block = Block.blocksList[i];
                if (block == Block.woodSingleSlab)
                    return 150;
                if (block.blockMaterial == Material.wood) 
                    return 300;
                if (block == Block.coalBlock)
                    return 16000;
            }
            if (item instanceof ItemTool && ((ItemTool) item).getToolMaterialName().equals("WOOD"))
                return 200;
            if (item instanceof ItemSword && ((ItemSword) item).getToolMaterialName().equals("WOOD"))
                return 200;
            if (item instanceof ItemHoe && ((ItemHoe) item).getMaterialName().equals("WOOD"))
                return 200;
            if (i == Item.stick.itemID)
                return 100;
            if (i == Item.coal.itemID)
                return 1600;
            if (i == Item.bucketLava.itemID)
                return 20000;
            if (i == Block.sapling.blockID)
                return 100;
            if (i == Item.blazeRod.itemID)
                return 2400;
            return GameRegistry.getFuelValue(stack);
        }
    }

    @Override
    public void onInventoryChanged() {
        queueWorkUpdate = true;
    }

    @Override
    public EnumBasics getType() {
        return EnumBasics.ALLOYSMELTER;
    }

    @Override
    public void onBlockPlacedBy(EntityLivingBase player, ItemStack item) {}
}
