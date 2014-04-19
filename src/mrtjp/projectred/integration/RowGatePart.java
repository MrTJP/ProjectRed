package mrtjp.projectred.integration;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import mrtjp.projectred.core.libmc.BasicUtils;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.integration.ArrayCommons.ITopArrayWire;
import mrtjp.projectred.transmission.*;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTTagCompound;

import java.util.Arrays;

public class RowGatePart extends SimpleGatePart implements IRedwirePart, ITopArrayWire
{
    public byte signal;

    @Override
    public String getType()
    {
        return "pr_rgate";
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        tag.setByte("signal", signal);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        signal = tag.getByte("signal");
    }

    @Override
    public void writeDesc(MCDataOutput packet)
    {
        super.writeDesc(packet);
        packet.writeByte(signal);
    }

    @Override
    public void readDesc(MCDataInput packet)
    {
        super.readDesc(packet);
        signal = packet.readByte();
    }

    @Override
    public void read(MCDataInput packet, int switch_key)
    {
        if (switch_key == 11)
        {
            signal = packet.readByte();
            if (Configurator.staticGates)
                tile().markRender();
        }
        else
            super.read(packet, switch_key);
    }

    @Override
    public void updateAndPropagate(TMultiPart prev, int mode)
    {
        int wireMask = wireMask(prev);
        if ((wireMask&2) != 0)
            _updateAndPropogate(prev, mode);
        else
            WirePropagator.addNeighborChange(new BlockCoord(tile()));
    }

    private void _updateAndPropogate(TMultiPart prev, int mode)
    {
        int oldSignal = signal&0xFF;
        if (mode == IWirePart$.MODULE$.DROPPING() && oldSignal == 0)
            return;

        int newSignal = calculateSignal();
        if (newSignal < oldSignal)
        {
            if (newSignal > 0)
                WirePropagator.propagateAnalogDrop(this);

            signal = 0;
            propogate(prev, IWirePart$.MODULE$.DROPPING());
        }
        else if (newSignal > oldSignal)
        {
            signal = (byte)newSignal;
            if (mode == IWirePart$.MODULE$.DROPPING())
                propogate(null, IWirePart$.MODULE$.RISING());
            else
                propogate(prev, IWirePart$.MODULE$.RISING());
        }
        else if (mode == IWirePart$.MODULE$.DROPPING())
            propogateTo(prev, IWirePart$.MODULE$.RISING());
        else if (mode == IWirePart$.MODULE$.FORCE())
            propogate(prev, IWirePart$.MODULE$.FORCED());
    }

    public int calculateSignal()
    {
        WirePropagator.setDustProvidePower(false);
        WirePropagator$.MODULE$.redwiresProvidePower_$eq(false);

        int s = 0;
        int i;
        for (int r = 0; r < 4; r++)
            if (toInternal(r)%2 != 0)
            {
                if ((connMap&1<<r) != 0)
                    i = calculateCornerSignal(r);
                else
                    i = calculateStraightSignal(r);

                if (i > s)
                    s = i;
            }

        WirePropagator.setDustProvidePower(true);
        WirePropagator$.MODULE$.redwiresProvidePower_$eq(true);

        return s;
    }

    @Override
    public int calculateCornerSignal(int r)
    {
        int absDir = Rotation.rotateSide(side(), r);

        BlockCoord pos = new BlockCoord(tile()).offset(absDir).offset(side());
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            return getPartSignal(t.partMap(absDir^1), Rotation.rotationTo(absDir^1, side()^1));

        return 0;
    }

    @Override
    public int calculateStraightSignal(int r)
    {
        int absDir = Rotation.rotateSide(side(), r);
        int s = 0;

        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null && (connMap&0x10<<r) != 0)
        {
            TMultiPart tp = t.partMap(side());
            if (tp != null)
                s = getPartSignal(tp, (r+2)%4);
        }
        else
        {
            int blockID = world().getBlockId(pos.x, pos.y, pos.z);
            if (blockID == Block.redstoneWire.blockID)
                return world().getBlockMetadata(pos.x, pos.y, pos.z)-1;
        }

        int i = calculateRedstoneSignal(r);
        if (i > s)
            s = i;

        return s;
    }

    @Override
    public int calculateRedstoneSignal(int r)
    {
        int absDir = Rotation.rotateSide(side(), r);

        int i = RedstoneInteractions.getPowerTo(this, absDir)*17;
        if (i > 0)
            return i;

        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        return world().getIndirectPowerLevelTo(pos.x, pos.y, pos.z, absDir)*17;
    }

    @Override
    public int getPartSignal(TMultiPart part, int r)
    {
        if (part instanceof IRedwirePart && ((IRedwirePart)part).isWireSide(r))
            return ((IRedwirePart)part).getRedwireSignal(r)-1;
        else if (part instanceof IRedwireEmitter)
            return ((IRedwireEmitter)part).getRedwireSignal(r);

        return 0;
    }

    @Override
    public void onSignalUpdate()
    {
        tile().markDirty();
        super.onChange();
        getWriteStream(11).writeByte(signal);
    }

    public void propogate(TMultiPart prev, int mode)
    {
        if (mode != IWirePart$.MODULE$.FORCED())
            WirePropagator.addPartChange(this);

        for (int r = 0; r < 4; r++)
            if (toInternal(r)%2 != 0)
                if ((connMap&1<<r) != 0)
                    propogateCorner(r, prev, mode);
                else
                    propogateStraight(r, prev, mode);
    }

    public void propogateCorner(int r, TMultiPart prev, int mode)
    {
        int absDir = Rotation.rotateSide(side(), r);
        BlockCoord pos = new BlockCoord(tile()).offset(absDir).offset(side());

        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
        {
            TMultiPart tp = t.partMap(absDir^1);
            if (tp == prev)
                return;
            if (propogateTo(tp, mode))
                return;
        }

        WirePropagator.addNeighborChange(pos);
    }

    public void propogateStraight(int r, TMultiPart prev, int mode)
    {
        int absDir = Rotation.rotateSide(side(), r);
        BlockCoord pos = new BlockCoord(tile()).offset(absDir);

        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
        {
            TMultiPart tp = t.partMap(side());
            if (tp == prev)
                return;
            if (propogateTo(tp, mode))
                return;
        }

        WirePropagator.addNeighborChange(pos);
    }

    public int wireMask(TMultiPart propogator)
    {
        if (propogator.tile() == null)
            return 3;

        if (sideDiff(propogator) == Rotation.rotateSide(side(), toAbsolute(0))>>1)
            return 1;

        return 2;
    }

    public int sideDiff(TMultiPart part)
    {
        int a = side()>>1;
        if (a != 0 && y() != part.y())
            return 0;
        if (a != 1 && z() != part.z())
            return 1;

        return 2;
    }

    public boolean propogateTo(TMultiPart part, int mode)
    {
        if (part instanceof IWirePart)
        {
            WirePropagator.propagateTo((IWirePart)part, this, mode);
            return true;
        }

        return false;
    }

    @Override
    public int getRedwireSignal(int side)
    {
        int r = toInternal(side);
        return r%2 == 0 ? getLogic().getOutput(this, r)*17 : signal&0xFF;
    }

    @Override
    public void onChange()
    {
        super.onChange();
        WirePropagator.propagateTo(this, IWirePart$.MODULE$.RISING());
    }

    @Override
    public boolean canConnectRedstone(int side)
    {
        if ((side&6) == (side()&6))
            return false;

        int r = relRot(side);
        if (r%2 != 0)
            return true;

        return getLogic().canConnect(this, r);
    }

    @Override
    public int weakPowerLevel(int side)
    {
        if ((side&6) == (side()&6))
            return 0;

        int r = relRot(side);
        if (r%2 != 0)
            return rsLevel(signal&0xFF);

        return super.weakPowerLevel(side);
    }

    public int rsLevel(int i)
    {
        if (WirePropagator$.MODULE$.redwiresProvidePower())
            return (i+16)/17;

        return 0;
    }

    @Override
    public boolean isWireSide(int side)
    {
        if (side < 0)
            return false;

        return toInternal(side)%2 != 0;
    }

    @Override
    public Cuboid6 getBounds()
    {
        return ArrayCommons.cBoxes[side()];
    }

    @Override
    public Iterable<Cuboid6> getOcclusionBoxes()
    {
        return Arrays.asList(ArrayCommons.oBoxes[side()]);
    }
}
