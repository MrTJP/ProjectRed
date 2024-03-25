package mrtjp.projectred.integration.item;

import codechicken.multipart.api.ItemMultipart;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;

public abstract class BaseGatePartItem extends ItemMultipart {

    protected final GateType gateType;

    public BaseGatePartItem(Properties properties, GateType gateType) {
        super(properties);
        this.gateType = gateType;
    }

    public GateType getGateType() {
        return gateType;
    }

    @Override
    public MultiPart newPart(MultipartPlaceContext context) {
        Direction side = context.getClickedFace();
        BlockPos onPos = context.getClickedPos().relative(side.getOpposite());
        if (!PlacementLib.canPlaceGateOnSide(context.getLevel(), onPos, side)) {
            return null;
        }
        GatePart gatePart = gateType.newPart();
        if (!gatePart.preparePlacement(context)) {
            return null;
        }
        return gatePart;
    }
}
