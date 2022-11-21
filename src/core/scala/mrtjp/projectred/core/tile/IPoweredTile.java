package mrtjp.projectred.core.tile;

import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.core.IPowerConnectable;
import mrtjp.projectred.core.PowerConductor;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;

public interface IPoweredTile extends IBlockEventTile, IConnectableTile, IPowerConnectable {

    static PowerConductor getExternalConductorForFaceConn(IPoweredTile poweredTile, int s, int edgeRot) {
        if (poweredTile.maskConnectsStraight(s, edgeRot)) {
            TMultiPart part = poweredTile.getStraight(s, edgeRot);
            if (part instanceof IPowerConnectable) {
                return ((IPowerConnectable) part).conductor(poweredTile.rotFromStraight(s, edgeRot));
            }
        }
        if (poweredTile.maskConnectsCorner(s, edgeRot)) {
            TMultiPart part = poweredTile.getCorner(s, edgeRot);
            if (part instanceof IPowerConnectable) {
                return ((IPowerConnectable) part).conductor(poweredTile.rotFromCorner(s, edgeRot));
            }
        }

        return null;
    }

    static PowerConductor getExternalConductorForCenterConn(IPoweredTile poweredTile, int s) {
        if (poweredTile.maskConnectsStraightCenter(s)) {
            TMultiPart part = poweredTile.getStraightCenter(s);
            if (part instanceof IPowerConnectable) {
                return ((IPowerConnectable) part).conductor(s^1);
            }

            TileEntity tile = poweredTile.getBlockLevel().getBlockEntity(poweredTile.posOfStraight(s));
            if (tile instanceof IPowerConnectable) {
                return ((IPowerConnectable) tile).conductor(s^1);
            }
        }

        return null;
    }

    @Override
    default World connWorld() {
        return getBlockLevel();
    }
}
