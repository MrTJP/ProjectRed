package mrtjp.projectred.exploration.world.gen

import com.mojang.serialization.Codec
import mrtjp.projectred.exploration.ExplorationContent
import net.minecraft.block.{Block, Blocks}
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.biome.Biome
import net.minecraft.world.chunk.IChunk
import net.minecraft.world.gen.carver.CaveWorldCarver
import net.minecraft.world.gen.feature.ProbabilityConfig
import org.apache.commons.lang3.mutable.MutableBoolean

import java.util
import java.util.{Random, function}
import scala.jdk.CollectionConverters._

class MarbleCaveWorldCarver(codec:Codec[ProbabilityConfig], genHeight:Int) extends CaveWorldCarver(codec, genHeight)
{
    protected val MARBLE = ExplorationContent.blockMarble.get().defaultBlockState()

    {
        val blockSet = Set.newBuilder[Block]
        blockSet ++= replaceableBlocks.asScala
        blockSet += ExplorationContent.blockMarble.get()
        replaceableBlocks = blockSet.result().asJava
    }

    override protected def carveBlock(p_230358_1_ :IChunk, p_230358_2_ :function.Function[BlockPos, Biome], p_230358_3_ :util.BitSet, p_230358_4_ :Random, p_230358_5_ :BlockPos.Mutable, p_230358_6_ :BlockPos.Mutable, p_230358_7_ :BlockPos.Mutable, p_230358_8_ :Int, p_230358_9_ :Int, p_230358_10_ :Int, p_230358_11_ :Int, p_230358_12_ :Int, p_230358_13_ :Int, p_230358_14_ :Int, p_230358_15_ :Int, p_230358_16_ :MutableBoolean):Boolean = {
        val result = super.carveBlock(p_230358_1_, p_230358_2_, p_230358_3_, p_230358_4_, p_230358_5_, p_230358_6_, p_230358_7_, p_230358_8_, p_230358_9_, p_230358_10_, p_230358_11_, p_230358_12_, p_230358_13_, p_230358_14_, p_230358_15_, p_230358_16_)

        if (result) { //this position was carved. Surround it with marble
            val pos = new BlockPos(p_230358_11_, p_230358_14_, p_230358_12_).mutable()
            val carvedState = p_230358_1_.getBlockState(pos)

            if (carvedState.is(Blocks.AIR) || carvedState.is(Blocks.CAVE_AIR)) {
                for (s <- 0 until 6) {
                    pos.set(p_230358_11_, p_230358_14_, p_230358_12_).move(Direction.values()(s))
                    val adjacentState = p_230358_1_.getBlockState(pos)
                    if (canReplaceBlock(adjacentState)) {
                        p_230358_1_.setBlockState(pos, MARBLE, false)
                    }
                }
            }
        }
        result
    }
}
