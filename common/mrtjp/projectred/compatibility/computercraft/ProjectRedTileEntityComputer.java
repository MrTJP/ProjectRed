package mrtjp.projectred.compatibility.computercraft;

import dan200.ComputerCraft;
import dan200.computer.shared.BlockComputerBase;
import dan200.computer.shared.TileEntityComputer;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.transmission.BundledCableCommons;

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
        return BundledCableCommons.unpackDigital(null, getBundledPowerOutput(BlockComputerBase.getLocalSide(side^1, ComputerCraft.Blocks.computer.getDirection(worldObj, xCoord, yCoord, zCoord))));
    }
}
