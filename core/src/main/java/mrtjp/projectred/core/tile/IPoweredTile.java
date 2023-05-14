package mrtjp.projectred.core.tile;

import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.core.power.IPowerConductorSource;
import mrtjp.projectred.core.power.IPowerConnectable;
import mrtjp.projectred.core.power.PowerConductor;
import net.minecraft.tileentity.TileEntity;

public interface IPoweredTile extends IBlockEventTile, IConnectableTile, IPowerConnectable, IPowerConductorSource {

    static PowerConductor getExternalConductorForFaceConn(IPoweredTile poweredTile, int s, int edgeRot) {
        if (poweredTile.maskConnectsStraight(s, edgeRot)) {
            TMultiPart part = poweredTile.getStraight(s, edgeRot);
            if (part instanceof IPowerConnectable) {
                return ((IPowerConnectable) part).getConductor(poweredTile.rotFromStraight(s, edgeRot));
            }
        }
        if (poweredTile.maskConnectsCorner(s, edgeRot)) {
            TMultiPart part = poweredTile.getCorner(s, edgeRot);
            if (part instanceof IPowerConnectable) {
                return ((IPowerConnectable) part).getConductor(poweredTile.rotFromCorner(s, edgeRot));
            }
        }

        return null;
    }

    static PowerConductor getExternalConductorForCenterConn(IPoweredTile poweredTile, int s) {
        if (poweredTile.maskConnectsStraightCenter(s)) {
            TMultiPart part = poweredTile.getStraightCenter(s);
            if (part instanceof IPowerConnectable) {
                return ((IPowerConnectable) part).getConductor(s^1);
            }

            TileEntity tile = poweredTile.getBlockLevel().getBlockEntity(poweredTile.posOfStraight(s));
            if (tile instanceof IPowerConnectable) {
                return ((IPowerConnectable) tile).getConductor(s^1);
            }
        }

        return null;
    }

}
