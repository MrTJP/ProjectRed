package mrtjp.projectred.integration;

import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.core.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class BundledGatePart extends SimpleGatePart implements IBundledEmitter
{
    public BundledGateLogic logic;

    @Override
    public BundledGateLogic getLogic()
    {
        return logic;
    }

    @Override
    public String getType()
    {
        return "pr_bgate";
    }

    @Override
    public boolean connectStraightOverride(int absDir) {
        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        TileEntity t = world().getBlockTileEntity(pos.x, pos.y, pos.z);
        if(t instanceof IBundledTile)
            return ((IBundledTile) t).canConnectBundled(absDir^1);

        return false;
    }

    public byte[] getBundledInput(int r)
    {
        r = toAbsolute(r);
        if ((connMap & 1 << r) != 0)
            return calculateBundledCornerSignal(r);
        else if ((connMap & 0x10 << r) != 0)
            return calculateBundledStraightSignal(r);
        else if ((connMap & 0x100 << r) != 0)
            return calculateBundledInternalSignal(r);

        return null;
    }

    public byte[] calculateBundledCornerSignal(int r)
    {
        int absDir = Rotation.rotateSide(side(), r);

        BlockCoord pos = new BlockCoord(tile()).offset(absDir).offset(side());
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            return getBundledPartSignal(t.partMap(absDir ^ 1), Rotation.rotationTo(absDir ^ 1, side() ^ 1));

        return null;
    }

    public byte[] calculateBundledStraightSignal(int r)
    {
        int absDir = Rotation.rotateSide(side(), r);

        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        TileEntity t = world().getBlockTileEntity(pos.x, pos.y, pos.z);
        if (t instanceof IBundledEmitter)
            return getBundledPartSignal(t, absDir ^ 1);
        else if (t instanceof TileMultipart)
            return getBundledPartSignal(((TileMultipart) t).partMap(side()), (r + 2) % 4);

        return null;
    }

    public byte[] calculateBundledInternalSignal(int r)
    {
        int absDir = Rotation.rotateSide(side(), r);

        TMultiPart tp = tile().partMap(absDir);
        return getBundledPartSignal(tp, Rotation.rotationTo(absDir, side()));
    }

    public byte[] getBundledPartSignal(Object part, int r)
    {
        if (part instanceof IBundledEmitter)
            return ((IBundledEmitter) part).getBundledSignal(r);

        return null;
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        logic = BundledGateLogic.create(this, subID);
        logic.load(tag);
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        logic.save(tag);
    }

    @Override
    public void writeDesc(MCDataOutput packet)
    {
        super.writeDesc(packet);
        logic.writeDesc(packet);
    }

    @Override
    public void readDesc(MCDataInput packet)
    {
        super.readDesc(packet);
        if (logic == null)
            logic = BundledGateLogic.create(this, subID);
        logic.readDesc(packet);
    }

    @Override
    public void read(MCDataInput packet, int switch_key)
    {
        if (switch_key <= 10)
            super.read(packet, switch_key);
        else
            logic.read(packet, switch_key);
    }

    @Override
    public void preparePlacement(EntityPlayer player, BlockCoord pos, int side, int meta)
    {
        super.preparePlacement(player, pos, side, meta);
        logic = BundledGateLogic.create(this, subID);
    }

    @Override
    public byte[] getBundledSignal(int r)
    {
        return getLogic().getBundledOutput(this, toInternal(r));
    }
}
