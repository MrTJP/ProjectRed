package mrtjp.projectred.compatibility.computercraft;

import dan200.ComputerCraft;
import dan200.computer.shared.BlockComputerBase;
import dan200.computer.shared.TileEntityComputer;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.transmission.BundledCommons;

public class ProjectRedTileEntityComputer extends TileEntityComputer implements IBundledTile
{
    public ProjectRedTileEntityComputer() {
    }

    public ProjectRedTileEntityComputer(boolean isClient) {
        super(isClient);
    }

    @Override
    public boolean canConnectBundled(int side) {
        return true;
    }

    @Override
    public byte[] getBundledSignal(int side) {
        return BundledCommons.unpackDigital(null, getBundledPowerOutput(BlockComputerBase.getLocalSide(side^1, ComputerCraft.Blocks.computer.getDirection(worldObj, xCoord, yCoord, zCoord))));
    }
}
