package mrtjp.projectred.expansion.part;

import codechicken.multipart.api.PartConverter;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Collection;
import java.util.Collections;

public class FramePartConverter extends PartConverter {

    public static final FramePartConverter INSTANCE = new FramePartConverter();

    private FramePartConverter() {
    }

    @Override
    public ConversionResult<Collection<MultiPart>> convert(LevelAccessor world, BlockPos pos, BlockState state) {
        if (state == ExpansionReferences.FRAME_BLOCK.defaultBlockState()) {
            return ConversionResult.success(Collections.singleton(new FramePart()));
        }
        return emptyResultList();
    }

    @Override
    public ConversionResult<MultiPart> convert(MultipartPlaceContext context) {
        if (context.getItemInHand().getItem() == ExpansionReferences.FRAME_BLOCK.asItem()) {
            return ConversionResult.success(new FramePart());
        }
        return emptyResult();
    }
}
