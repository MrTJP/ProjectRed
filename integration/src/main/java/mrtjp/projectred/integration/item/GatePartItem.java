package mrtjp.projectred.integration.item;

import codechicken.multipart.api.ItemMultipart;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.ProjectRedIntegration;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.Item;

public class GatePartItem extends ItemMultipart {

    private final GateType gateType;

    public GatePartItem(GateType gateType) {
        super(new Item.Properties().tab(ProjectRedIntegration.CREATIVE_TAB));
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
        gatePart.preparePlacement(context.getPlayer(), onPos, side.ordinal());
        return gatePart;
    }
}
