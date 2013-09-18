package mrtjp.projectred.illumination;

import mrtjp.projectred.core.CoreSPH;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraft.tileentity.TileEntity;
import codechicken.lib.packet.ICustomPacketTile;
import codechicken.lib.packet.PacketCustom;

public class TileLamp extends TileEntity implements ICustomPacketTile, ILight {

    public boolean inverted;
    public boolean powered;
    public byte color;
    
    public TileLamp() {}
    
    private boolean firstTick = true;
    
    public void prepairPlacement(boolean inverted, int meta) {
        this.color = (byte) meta;
        this.inverted = inverted;
    }
            
    public int getLightValue() {
        return powered != inverted ? 15 : 0;
    }

    public ItemStack getDroppedBlock() {
        return new ItemStack(worldObj.getBlockId(xCoord, yCoord, zCoord), 1, color+(inverted?16:0));
    }

    public void onNeighborBlockChange() {
        if(!worldObj.isRemote && powered != isBeingPowered()) {
            powered = !powered;
            updateRender();
        }
    }
    
    public void updateRender() {
        worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
        worldObj.updateAllLightTypes(xCoord, yCoord, zCoord);
    }
    
    @Override
    public void updateEntity() {
    	if (firstTick) {
    		onNeighborBlockChange();
    		firstTick = false;
    	}
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
    	PacketCustom packet = new PacketCustom(CoreSPH.channel, 1);
    	packet.writeCoord(xCoord, yCoord, zCoord);
    	int pack = color;
    	if(inverted) pack |= 0x10;
    	if(powered) pack |= 0x20;
    	packet.writeByte(pack);
    	return packet.toPacket();
    }
    
    @Override
    public void handleDescriptionPacket(PacketCustom packet) {
        int packed = packet.readUByte();
        color = (byte) (packed & 0xF);
        inverted = (packed & 0x10) != 0;
        powered = (packed & 0x20) != 0;
        updateRender();
    }

    private boolean isBeingPowered() {
        return worldObj.isBlockIndirectlyGettingPowered(xCoord, yCoord, zCoord);
    }

    @Override
    public boolean isOn() {
        return getLightValue() == 15;
    }
}
