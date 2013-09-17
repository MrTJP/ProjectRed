package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.tileentity.TileEntity;
import codechicken.lib.packet.PacketCustom;

public class TileLamp extends TileEntity implements ILight {

    public boolean inverted;
    public boolean powered;
    public byte color;
    
    public TileLamp() {}
    
    public void prepairPlacement(boolean inverted, int meta) {
        this.color = (byte) meta;
        this.inverted = inverted;
    }
            
    public int getLightValue() {
        if (powered != inverted)
            return 15;
        else
            return 0;
    }

    public ItemStack getDroppedBlock() {
        return new ItemStack(worldObj.getBlockId(xCoord, yCoord, zCoord), 1, color+(inverted?16:0));
    }

    public void onNeighborBlockChange() {
        updateState(false);
    }

    public void updateState(boolean forceRender) {
        boolean updated = false;
        if (!powered && isBeingPowered()) {
            powered = true;
            updateRender();
            updated = true;
        } else if (powered && !isBeingPowered()){
            powered = false;
            updateRender();
            updated = true;
        }
        if (forceRender && !updated)
            updateRender();
    }
    
    public void updateRender() {
        worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
        worldObj.updateAllLightTypes(xCoord, yCoord, zCoord);
    }
    
    @Override
    public void writeToNBT(NBTTagCompound nbt) {
        super.writeToNBT(nbt);
        nbt.setBoolean("inverted", inverted);
        nbt.setBoolean("powered", powered);
        nbt.setByte("meta", color);
    }

    @Override
    public void readFromNBT(NBTTagCompound nbt) {
        super.readFromNBT(nbt);
        inverted = nbt.getBoolean("inverted");
        powered = nbt.getBoolean("powered");
        color = nbt.getByte("meta");
    }

    @Override
    public Packet getDescriptionPacket() {
    	PacketCustom packet = new PacketCustom(ProjectRedIllumination.instance, 1);
    	packet.writeCoord(xCoord, yCoord, zCoord);
    	packet.writeBoolean(inverted);
    	packet.writeBoolean(powered);
    	packet.writeByte(color);
    	return packet.toPacket();
    }

    private boolean isBeingPowered() {
        return worldObj.isBlockIndirectlyGettingPowered(xCoord, yCoord, zCoord);
    }

    @Override
    public boolean isOn() {
        return getLightValue() == 15;
    }
}
