package mrtjp.projectred.integration.item;

import codechicken.multipart.api.ItemMultiPart;
import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.PRLib;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraft.item.Item;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;

public class GatePartItem extends ItemMultiPart {

    private final GateType gateType;

    public GatePartItem(GateType gateType) {
        super(new Item.Properties().tab(ProjectRedIntegration.INTEGRATION_GROUP));
        this.gateType = gateType;
    }

    public GateType getGateType() {
        return gateType;
    }

    @Override
    public TMultiPart newPart(ItemUseContext context) {
        Direction side = context.getClickedFace();
        BlockPos onPos = context.getClickedPos().relative(side.getOpposite());
        if (!PRLib.canPlaceGateOnSide(context.getLevel(), onPos, side)) {
            return null;
        }
        GatePart gatePart = gateType.newPart();
        gatePart.preparePlacement(context.getPlayer(), onPos, side.ordinal());
        return gatePart;
    }
}
