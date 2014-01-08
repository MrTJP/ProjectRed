package mrtjp.projectred.compatibility.computercraft;

import dan200.computer.shared.BlockComputerBase;
import dan200.turtle.shared.TileEntityTurtle;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.transmission.BundledCableCommons;

public class ProjectRedTileEntityTurtle extends TileEntityTurtle implements IBundledTile
{
    public ProjectRedTileEntityTurtle() {
    }

    public ProjectRedTileEntityTurtle(boolean isClient, int _dir) {
        super(isClient, _dir);
    }

    @Override
    public boolean canConnectBundled(int side) {
        return !hasPeripheralUpgradeOnSide(BlockComputerBase.getLocalSide(side, getFacingDir()));
    }

    @Override
    public byte[] getBundledSignal(int side) {
        return BundledCableCommons.unpackDigital(null, getBundledPowerOutput(BlockComputerBase.getLocalSide(side^1, getFacingDir())));
    }
}
