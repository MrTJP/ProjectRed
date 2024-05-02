package mrtjp.projectred.expansion.item;

import codechicken.multipart.api.ItemMultipart;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.projectred.expansion.TubeType;
import mrtjp.projectred.expansion.part.BaseTubePart;
import net.minecraft.world.item.Item;

public class TubePartItem extends ItemMultipart {

    private final TubeType type;

    public TubePartItem(TubeType type) {
        super(new Item.Properties());
        this.type = type;
    }

    public TubeType getType() {
        return type;
    }

    @Override
    public MultiPart newPart(MultipartPlaceContext context) {
        BaseTubePart pipe = type.newPart();
        if (pipe.preparePlacement(context)) {
            return pipe;
        }

        return null;
    }
}
