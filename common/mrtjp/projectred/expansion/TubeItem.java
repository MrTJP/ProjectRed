package mrtjp.projectred.expansion;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;

public class TubeItem {

    public int progress = 0;
    
    public int speed = 20;
    public byte direction;
    public byte color;
    public boolean hasCrossedCenter;
    public boolean pathFoundByParent;
    
    public ItemStack item;
    
    public TubeItem(){};
    
    public TubeItem(ItemStack item, int initialDirection) {
        this.item = item;
        this.direction = (byte) initialDirection;
    }
    
    public void resetProgress() {
        progress -= 100;
        hasCrossedCenter = false;
        pathFoundByParent = false;
    }
    
    public void update() {
        int oldProg = progress;
        progress += speed;
        boolean isAtCenter = oldProg < 50 && progress >= 50;
        if (isAtCenter) {
            hasCrossedCenter = true;
        }
    }
    
    public void read(NBTTagCompound nbt) {
        this.progress = nbt.getInteger("prog");
        this.speed = nbt.getInteger("spd");
        this.direction = nbt.getByte("dir");
        this.color = nbt.getByte("col");
        this.hasCrossedCenter = nbt.getBoolean("cent");
        this.pathFoundByParent = nbt.getBoolean("route");
        this.item = ItemStack.loadItemStackFromNBT(nbt);
    }
    
    public void write(NBTTagCompound nbt) {
        nbt.setInteger("prog", progress);
        nbt.setInteger("spd", speed);
        nbt.setByte("dir", direction);
        nbt.setByte("col", color);
        nbt.setBoolean("cent", hasCrossedCenter);
        nbt.setBoolean("route", pathFoundByParent);
        item.writeToNBT(nbt);
    }
    
    public static TubeItem fromNBT(NBTTagCompound nbt) {
        TubeItem ti = new TubeItem();
        ti.read(nbt);
        return ti;
    }
}
