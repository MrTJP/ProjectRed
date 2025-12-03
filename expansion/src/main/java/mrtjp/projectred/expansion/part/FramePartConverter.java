package mrtjp.projectred.expansion.part;

import codechicken.multipart.api.PartConverter;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.util.MultipartPlaceContext;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.shapes.CollisionContext;

import java.util.Collection;
import java.util.Collections;

public class FramePartConverter extends PartConverter {

    public static final FramePartConverter INSTANCE = new FramePartConverter();

    private FramePartConverter() {
    }

    @Override
    public ConversionResult<Collection<MultiPart>> convert(LevelAccessor world, BlockPos pos, BlockState state) {
        if (state == ExpansionBlocks.FRAME_BLOCK.get().defaultBlockState()) {
            return ConversionResult.success(Collections.singleton(new FramePart()));
        }
        return emptyResultList();
    }

    @Override
    public ConversionResult<MultiPart> convert(MultipartPlaceContext context) {
        if (context.getItemInHand().getItem() == ExpansionBlocks.FRAME_BLOCK.get().asItem()) {
            BlockState state = ExpansionBlocks.FRAME_BLOCK.get().getStateForPlacement(context);
            if (state != null && canPlace(context, state)) {
                return ConversionResult.success(new FramePart());
            }
        }
        return emptyResult();
    }

    // Lifted from BlockItem
    private boolean canPlace(BlockPlaceContext context, BlockState state) {
        Player player = context.getPlayer();
        CollisionContext collisioncontext = player == null ? CollisionContext.empty() : CollisionContext.of(player);
        return (state.canSurvive(context.getLevel(), context.getClickedPos()))
                && context.getLevel().isUnobstructed(state, context.getClickedPos(), collisioncontext);
    }
}
