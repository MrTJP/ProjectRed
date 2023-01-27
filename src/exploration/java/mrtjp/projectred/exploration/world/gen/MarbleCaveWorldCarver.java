package mrtjp.projectred.exploration.world.gen;

import com.mojang.serialization.Codec;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.chunk.IChunk;
import net.minecraft.world.gen.carver.CaveWorldCarver;
import net.minecraft.world.gen.feature.ProbabilityConfig;
import org.apache.commons.lang3.mutable.MutableBoolean;

import java.util.BitSet;
import java.util.Random;
import java.util.function.Function;

import static mrtjp.projectred.exploration.init.ExplorationReferences.MARBLE_BLOCK;

public class MarbleCaveWorldCarver extends CaveWorldCarver {

    public MarbleCaveWorldCarver(Codec<ProbabilityConfig> codec, int genHeight) {
        super(codec, genHeight);
    }

    @Override
    protected boolean carveBlock(IChunk p_230358_1_, Function<BlockPos, Biome> p_230358_2_, BitSet p_230358_3_, Random p_230358_4_, BlockPos.Mutable p_230358_5_, BlockPos.Mutable p_230358_6_, BlockPos.Mutable p_230358_7_, int p_230358_8_, int p_230358_9_, int p_230358_10_, int p_230358_11_, int p_230358_12_, int p_230358_13_, int p_230358_14_, int p_230358_15_, MutableBoolean p_230358_16_) {
        if (!super.carveBlock(p_230358_1_, p_230358_2_, p_230358_3_, p_230358_4_, p_230358_5_, p_230358_6_, p_230358_7_, p_230358_8_, p_230358_9_, p_230358_10_, p_230358_11_, p_230358_12_, p_230358_13_, p_230358_14_, p_230358_15_, p_230358_16_)) {
            return false;
        }

        // This position was carved. Surround it with marble

        BlockPos.Mutable pos = new BlockPos(p_230358_11_, p_230358_14_, p_230358_12_).mutable();
        BlockState carvedState = p_230358_1_.getBlockState(pos);

        if (carvedState.is(Blocks.AIR) || carvedState.is(Blocks.CAVE_AIR)) {

            BlockState marbleState = MARBLE_BLOCK.defaultBlockState();

            for (int s = 0; s < 6; s++) {
                pos.set(p_230358_11_, p_230358_14_, p_230358_12_).move(Direction.values()[s]);
                BlockState adjacentState = p_230358_1_.getBlockState(pos);
                if (canReplaceBlock(adjacentState)) {
                    p_230358_1_.setBlockState(pos, marbleState, false);
                }
            }
        }

        return true;
    }
}
