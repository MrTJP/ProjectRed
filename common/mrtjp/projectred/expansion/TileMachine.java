package mrtjp.projectred.expansion;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.core.ItemScrewdriver;
import mrtjp.projectred.core.blockutil.TileMulti;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;

public abstract class TileMachine extends TileMulti
{
    protected byte rotation;
    private final int blockID;

    public TileMachine(int id)
    {
        blockID = id;
    }

    @Override
    public void onBlockPlaced(ItemStack ist, int side, EntityPlayer ent)
    {
        rotation = (byte) resolveLook(ent);
    }

    @Override
    public int getBlockID()
    {
        return blockID;
    }

    @Override
    public abstract int getBlockMetadata();

    @Override
    public void writeDesc(MCDataOutput out)
    {
        super.writeDesc(out);
        out.writeByte(rotation);
    }

    @Override
    public void readDesc(MCDataInput in)
    {
        super.readDesc(in);
        rotation = in.readByte();
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        tag.setByte("rot", rotation);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        rotation = tag.getByte("rot");
    }

    @Override
    public void read(MCDataInput in, int switchkey)
    {
        if (switchkey == 1)
            rotation = in.readByte();
        else
            super.read(in, switchkey);
    }

    @Override
    public boolean onBlockActivated(EntityPlayer player)
    {
        ItemStack held = player.getHeldItem();
        if (held != null && held.getItem() instanceof ItemScrewdriver)
        {
            if (worldObj.isRemote)
                return true;
            int old = rotation;
            do
                rotation = (byte) ((rotation + 1) % 6);
            while (!isRotationAllowed(rotation) && rotation != old);

            if (rotation != old)
                sendRotationUpdate();

            return true;
        }
        return false;
    }

    protected void sendRotationUpdate()
    {
        writeStream(1).writeByte(rotation).sendToChunk(worldObj, xCoord / 16, zCoord / 16);
    }

    protected boolean isRotationAllowed(int rot)
    {
        return true;
    }

    public int resolveLook(EntityPlayer player)
    {
        int rot = (int) Math.floor(player.rotationYaw * 4.0F / 360.0F + 0.5D) & 0x3;
        if (Math.abs(player.posX - xCoord) < 2D && Math.abs(player.posZ - zCoord) < 2D)
        {
            double p = player.posY + 1.82D - player.yOffset - this.yCoord;
            if (p > 2.0D)
                return 0;
            if (p < 0.0D)
                return 1;
        }
        switch (rot) {
            case 0:
                return 3;
            case 1:
                return 4;
            case 2:
                return 2;
            case 3:
                return 5;
        }
        return 1;
    }
}
