package mrtjp.projectred.illumination.item;

import codechicken.multipart.api.ItemMultiPart;
import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.illumination.MultipartLightProperties;
import mrtjp.projectred.illumination.part.MultipartLightPart;
import net.minecraft.item.Item;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;

public class MultipartLightPartItem extends ItemMultiPart {

    private final MultipartLightProperties properties;
    private final int color;
    private final boolean inverted;

    public MultipartLightPartItem(MultipartLightProperties lightProperties, int color, boolean inverted) {
        super(new Item.Properties().tab(ProjectRedIllumination.ILLUMINATION_GROUP));
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
    public TMultiPart newPart(ItemUseContext context) {
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
