package mrtjp.projectred.transmission.item;

import codechicken.multipart.api.ItemMultiPart;
import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.ProjectRedTransmission;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.part.BaseWirePart;
import net.minecraft.item.Item;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;

public class FaceWirePartItem extends ItemMultiPart {

    private final WireType type;

    public FaceWirePartItem(WireType type) {
        super(new Item.Properties().tab(ProjectRedTransmission.TRANSMISSION_GROUP));
        this.type = type;
    }

    public WireType getType() {
        return type;
    }

    @Override
    public TMultiPart newPart(ItemUseContext context) {
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
