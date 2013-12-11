package mrtjp.projectred.illumination;

import mrtjp.projectred.core.CoreSPH;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraft.tileentity.TileEntity;
import codechicken.lib.packet.ICustomPacketTile;
import codechicken.lib.packet.PacketCustom;

public class TileLamp extends TileEntity implements ICustomPacketTile, ILight
{
    public boolean inverted;
    public boolean powered;

    @Override
    public boolean canUpdate()
    {
        return false;
    }

    public void prepairPlacement(boolean inverted, int meta)
    {
        this.inverted = inverted;
    }

    public int getLightValue()
    {
        return powered != inverted ? 15 : 0;
    }

    public int colour()
    {
        return getBlockMetadata();
    }

    public ItemStack getDroppedBlock()
    {
        return new ItemStack(worldObj.getBlockId(xCoord, yCoord, zCoord), 1, colour() + (inverted ? 16 : 0));
    }

    public void onNeighborBlockChange()
    {
        if (!worldObj.isRemote && powered != isBeingPowered())
            worldObj.scheduleBlockUpdate(xCoord, yCoord, zCoord, getBlockType().blockID, 2);
    }

    public void onTick()
    {
        if (powered != isBeingPowered())
        {
            powered = !powered;
            updateRender();
        }
    }

    public void updateRender()
    {
        worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
        worldObj.updateAllLightTypes(xCoord, yCoord, zCoord);
    }

    @Override
    public void writeToNBT(NBTTagCompound nbt)
    {
        super.writeToNBT(nbt);
        nbt.setBoolean("inverted", inverted);
        nbt.setBoolean("powered", powered);
    }

    @Override
    public void readFromNBT(NBTTagCompound nbt)
    {
        super.readFromNBT(nbt);
        inverted = nbt.getBoolean("inverted");
        powered = nbt.getBoolean("powered");
    }

    @Override
    public Packet getDescriptionPacket()
    {
        PacketCustom packet = new PacketCustom(CoreSPH.channel, 1);
        packet.writeCoord(xCoord, yCoord, zCoord);
        int pack = 0;
        if (inverted)
            pack |= 0x1;
        if (powered)
            pack |= 0x2;
        packet.writeByte(pack);
        return packet.toPacket();
    }

    @Override
    public void handleDescriptionPacket(PacketCustom packet)
    {
        int packed = packet.readUByte();
        inverted = (packed & 0x1) != 0;
        powered = (packed & 0x2) != 0;
        updateRender();
    }

    private boolean isBeingPowered()
    {
        return worldObj.isBlockIndirectlyGettingPowered(xCoord, yCoord, zCoord);
    }

    @Override
    public boolean isOn()
    {
        return getLightValue() == 15;
    }
}
