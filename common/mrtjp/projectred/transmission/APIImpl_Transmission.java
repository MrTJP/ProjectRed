package mrtjp.projectred.transmission;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.api.ISpecialLinkState;
import mrtjp.projectred.api.ITransmissionAPI;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class APIImpl_Transmission implements ITransmissionAPI
{
    @Override
    public byte[] getBundledInput(World world, int x, int y, int z, int side)
    {
        BlockCoord pos = new BlockCoord(x, y, z).offset(side);
        TileEntity t = world.getBlockTileEntity(pos.x, pos.y, pos.z);
        if (t instanceof IBundledTile)
            return ((IBundledTile) t).getBundledSignal(side ^ 1);
        else if (t instanceof TileMultipart)
        {
            TileMultipart tmp = (TileMultipart) t;
            byte[] signal = null;
            for (int r = 0; r < 4; r++)
            {
                int pside = Rotation.rotateSide(side, r);
                TMultiPart p = tmp.partMap(pside);
                if (p instanceof IBundledEmitter)
                    signal = BundledCableCommons.raiseSignal(signal, ((IBundledEmitter) p).getBundledSignal(Rotation.rotationTo(pside, side ^ 1)));
            }

            TMultiPart p = tmp.partMap(6);
            if (p instanceof IBundledEmitter)
                signal = BundledCableCommons.raiseSignal(signal, ((IBundledEmitter) p).getBundledSignal(side ^ 1));

            return signal;
        }

        return null;
    }
}
