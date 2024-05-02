package mrtjp.projectred.illumination.item;

import codechicken.multipart.api.ItemMultipart;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.projectred.illumination.MultipartLightProperties;
import mrtjp.projectred.illumination.part.MultipartLightPart;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.Item;

public class MultipartLightPartItem extends ItemMultipart {

    private final MultipartLightProperties properties;
    private final int color;
    private final boolean inverted;

    public MultipartLightPartItem(MultipartLightProperties lightProperties, int color, boolean inverted) {
        super(new Item.Properties());
        this.properties = lightProperties;
        this.color = color;
        this.inverted = inverted;
    }

    public int getColor() {
        return color;
    }

    public boolean isInverted() {
        return inverted;
    }

    @Override
    public MultiPart newPart(MultipartPlaceContext context) {
        Direction side = context.getClickedFace();
        BlockPos onPos = context.getClickedPos().relative(side.getOpposite());

        if (properties.canFloat() || MultipartLightPart.canPlaceLight(context.getLevel(), onPos, side)) {
            MultipartLightPart part = properties.partFactory(color, inverted);
            part.preparePlacement(side.getOpposite().ordinal());
            return part;
        }

        return null;
    }
}
