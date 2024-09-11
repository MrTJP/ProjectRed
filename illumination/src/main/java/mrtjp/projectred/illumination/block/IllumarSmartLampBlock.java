package mrtjp.projectred.illumination.block;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.illumination.init.IlluminationBlocks;
import mrtjp.projectred.illumination.tile.IllumarSmartLampBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.EntityBlock;
import net.minecraft.world.level.block.LightBlock;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.IntegerProperty;
import net.minecraft.world.level.material.MapColor;
import org.jetbrains.annotations.Nullable;

import java.util.function.ToIntFunction;

public class IllumarSmartLampBlock extends ProjectRedBlock implements EntityBlock {

    public static final IntegerProperty LEVEL = LightBlock.LEVEL;
    public static final ToIntFunction<BlockState> LIGHT_EMISSION = LightBlock.LIGHT_EMISSION;

    public IllumarSmartLampBlock() {
        super(BlockBehaviour.Properties.of()
                .mapColor(state -> state.getValue(LEVEL) > 0 ? MapColor.TERRACOTTA_WHITE : MapColor.COLOR_GRAY) // TODO Actual color
                .sound(SoundType.GLASS)
                .strength(0.5F)
                .lightLevel(LIGHT_EMISSION));

        this.registerDefaultState(this.stateDefinition.any().setValue(LEVEL, 0));
    }

    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> pBuilder) {
        pBuilder.add(LEVEL).add(ProjectRedBlock.SIDE);
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext pContext) {
        int s = pContext.getClickedFace().ordinal() ^ 1; // Place bottom of block on the side clicked
        return defaultBlockState()
                .setValue(LEVEL, 0)
                .setValue(SIDE, s);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return IlluminationBlocks.ILLUMAR_SMART_LAMP_BLOCK_ENTITY.get();
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pPos, BlockState pState) {
        return new IllumarSmartLampBlockEntity(pPos, pState);
    }
}
