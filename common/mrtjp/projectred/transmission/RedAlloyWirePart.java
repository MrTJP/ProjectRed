package mrtjp.projectred.transmission;

import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;

public class RedAlloyWirePart extends RedwirePart
{
    @Override
    public WireDef getWireType()
    {
        return WireDefs.RED_ALLOY();
    }

    @Override
    public int getColour()
    {
        return (signal&0xFF)/2+60<<24|0xFF;
    }

    @Override
    public int strongPowerLevel(int side)
    {
        if (side == this.side)
            return rsLevel();

        return 0;
    }

    @Override
    public int redstoneConductionMap()
    {
        return 0x1F;
    }

    @Override
    public void onRemoved()
    {
        super.onRemoved();
        if (!world().isRemote)
            tile().notifyNeighborChange(side);
    }

    @Override
    public void propogateOther(int mode)
    {
        WirePropogator.addNeighborChange(new BlockCoord(tile()).offset(side));
        WirePropogator.addNeighborChange(new BlockCoord(tile()).offset(side^1));
        for (int r = 0; r < 4; r++)
            if (!maskConnects(r))
                WirePropogator.addNeighborChange(new BlockCoord(tile()).offset(Rotation.rotateSide(side, r)));

        for (int s = 0; s < 6; s++)
            if (s != (side^1))
                WirePropogator.addNeighborChange(new BlockCoord(tile()).offset(side).offset(s));
    }

    @Override
    public int calculateUndersideSignal()
    {
        BlockCoord pos = new BlockCoord(tile()).offset(side);
        return world().getIndirectPowerLevelTo(pos.x, pos.y, pos.z, side)*17;
    }
}
