package mrtjp.projectred.illumination;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public class TileAirousLight extends TileEntity
{
    private BlockCoord source;
    private int sourcePartID = -1;
    private int color = -1;
    
    private int delay = 100;

    @Override
    public void updateEntity()
    {
        if (!worldObj.isRemote)
        {
            if (--delay > 0)
                return;
            delay = worldObj.rand.nextInt(100);

            ILight tile = getLight();
            
            if (tile == null || !tile.isOn() || tile.getColor() != color)
                worldObj.setBlockToAir(xCoord, yCoord, zCoord);
        }
    }
    
    private ILight getLight()
    {
        if (sourcePartID > -1)
        {
            TMultiPart part = BasicUtils.getMultiPart(worldObj, source, sourcePartID);
            if (part instanceof ILight)
                return (ILight) part;
        }
        
        return BasicUtils.getTileEntity(worldObj, source, ILight.class);
    }
    
    public TileAirousLight setSource(BlockCoord source, int color, int partID)
    {
        this.source = source;
        this.color = color;
        sourcePartID = partID;
        return this;
    }
    
    public void readFromNBT(NBTTagCompound tag)
    {
        super.readFromNBT(tag);
        
        int x = tag.getInteger("sX");
        int y = tag.getInteger("sY");
        int z = tag.getInteger("sZ");
        
        sourcePartID = tag.getByte("spID");
        color = tag.getByte("col");
        
        source = new BlockCoord(x, y, z);
    }

    public void writeToNBT(NBTTagCompound tag)
    {
        super.writeToNBT(tag);

        tag.setInteger("sX", source.x);
        tag.setInteger("sY", source.y);
        tag.setInteger("sX", source.z);
        
        tag.setByte("spID", (byte) sourcePartID);
        tag.setByte("col", (byte) color);
    }
    
    public int getColor()
    {
        return color;
    }
}
