package mrtjp.projectred.transmission.item;

import codechicken.multipart.api.ItemMultipart;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.transmission.ProjectRedTransmission;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.part.BaseWirePart;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.Item;

public class FaceWirePartItem extends ItemMultipart {

    private final WireType type;

    public FaceWirePartItem(WireType type) {
        super(new Item.Properties().tab(ProjectRedTransmission.TRANSMISSION_GROUP));
        this.type = type;
    }

    public WireType getType() {
        return type;
    }

    @Override
    public MultiPart newPart(MultipartPlaceContext context) {
        Direction side = context.getClickedFace();
        BlockPos onPos = context.getClickedPos().relative(side.getOpposite());
        if (!PlacementLib.canPlaceWireOnSide(context.getLevel(), onPos, side)) {
            return null;
        } else {
            BaseWirePart wire = type.newPart();
            wire.preparePlacement(side);
            return wire;
        }
    }
}
