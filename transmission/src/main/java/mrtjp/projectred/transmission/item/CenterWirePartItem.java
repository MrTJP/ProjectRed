package mrtjp.projectred.transmission.item;

import codechicken.multipart.api.ItemMultiPart;
import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.transmission.ProjectRedTransmission;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.part.BaseWirePart;
import net.minecraft.item.Item;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.Direction;

public class CenterWirePartItem extends ItemMultiPart {

    private final WireType type;

    public CenterWirePartItem(WireType type) {
        super(new Item.Properties().tab(ProjectRedTransmission.TRANSMISSION_GROUP));
        this.type = type;
    }

    public WireType getType() {
        return type;
    }

    @Override
    public TMultiPart newPart(ItemUseContext context) {
        Direction side = context.getClickedFace();
        BaseWirePart wire = type.newPart();
        wire.preparePlacement(side);
        return wire;
    }
}
