package mrtjp.projectred.core;

import java.util.ArrayList;

import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;

public abstract class TileMulti extends TileEntity {
    protected long timeSched = -1L;

    public void onBlockNeighborChange(int l) {
    }

    public void onBlockPlaced(ItemStack ist, int side, EntityPlayer ent) {
    }

    public void onBlockRemoval() {
    }

    public int isBlockStrongPoweringTo(int side) {
        return 0;
    }

    public int isBlockWeakPoweringTo(int side) {
        return isBlockStrongPoweringTo(side);
    }

    public boolean onBlockActivated(EntityPlayer player) {
        return false;
    }

    public void onEntityCollidedWithBlock(Entity ent) {
    }

    public AxisAlignedBB getCollisionBoundingBox() {
        return null;
    }

    public void onTileTick() {
    }

    public int getMetaData() {
        return 0;
    }

    public abstract int getBlockID();

    public int getExtendedMetadata() {
        return 0;
    }

    public void setExtendedMetadata(int md) {
    }

    public void addHarvestContents(ArrayList ist) {
        ist.add(new ItemStack(getBlockID(), 1, getMetaData()));
    }

    public void scheduleTick(int time) {
        long tn = this.worldObj.getWorldTime() + time;
        if ((this.timeSched > 0L) && (this.timeSched < tn))
            return;
        this.timeSched = tn;
        dirtyBlock();
    }

    public boolean isTickRunnable() {
        return (this.timeSched >= 0L) && (this.timeSched <= this.worldObj.getWorldTime());
    }

    public boolean isTickScheduled() {
        return this.timeSched >= 0L;
    }

    public void updateBlockChange() {
        BasicUtils.updateIndirectNeighbors(worldObj, xCoord, yCoord, zCoord, getBlockID());

        this.worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
        BasicUtils.markBlockDirty(worldObj, xCoord, yCoord, zCoord);
    }

    public void updateBlock() {
        int md = this.worldObj.getBlockMetadata(xCoord, yCoord, zCoord);
        this.worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
        BasicUtils.markBlockDirty(worldObj, xCoord, yCoord, zCoord);
    }

    public void dirtyBlock() {
        BasicUtils.markBlockDirty(worldObj, xCoord, yCoord, zCoord);
    }

    public void breakBlock() {
        ArrayList<ItemStack> il = new ArrayList<ItemStack>();
        addHarvestContents(il);
        for (ItemStack it : il)
            BasicUtils.dropItem(worldObj, xCoord, yCoord, zCoord, it);
        this.worldObj.setBlock(xCoord, yCoord, zCoord, 0);
    }

    public void updateEntity() {
        if (BasicUtils.isClient(this.worldObj))
            return;
        if (this.timeSched < 0L)
            return;
        long wtime = this.worldObj.getWorldTime();
        if (this.timeSched > wtime + 1200L) {
            this.timeSched = (wtime + 1200L);
        } else if (this.timeSched <= wtime) {
            this.timeSched = -1L;
            onTileTick();
            dirtyBlock();
        }
    }

    public void readFromNBT(NBTTagCompound nbttagcompound) {
        super.readFromNBT(nbttagcompound);
        this.timeSched = nbttagcompound.getLong("sched");
    }

    public void writeToNBT(NBTTagCompound nbttagcompound) {
        super.writeToNBT(nbttagcompound);
        nbttagcompound.setLong("sched", this.timeSched);
    }
}
