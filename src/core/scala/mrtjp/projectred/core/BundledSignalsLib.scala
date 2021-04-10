package mrtjp.projectred.core

import codechicken.lib.vec.Rotation
import codechicken.multipart.block.TileMultiPart
import mrtjp.projectred.api.{IBundledEmitter, IBundledTile, IBundledTileInteraction}
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

object BundledSignalsLib
{
    var interactions = Vector[IBundledTileInteraction]()

    def registerBundledTileInteraction(interaction:IBundledTileInteraction):Unit = {
        interactions :+= interaction
    }

    def getBundledInput(world:World, pos:BlockPos, facing:Direction):Array[Byte] = {
        val side = facing.ordinal
        world.getTileEntity(pos.offset(Direction.byIndex(side))) match {
            case ibt:IBundledTile => ibt.getBundledSignal(side^1)

            case tmp:TileMultiPart =>
                var signal:Array[Byte] = null
                def raise(ibe:IBundledEmitter, r:Int) {
                    signal = BundledCommons.raiseSignal(signal, ibe.getBundledSignal(r))
                }

                for (r <- 0 until 4) {
                    val pside = Rotation.rotateSide(side, r)
                    tmp.getSlottedPart(pside) match {
                        case ibe:IBundledEmitter => raise(ibe, Rotation.rotationTo(pside, side^1))
                        case _ =>
                    }
                }
                tmp.getSlottedPart(6) match {
                    case ibe:IBundledEmitter => raise(ibe, side^1)
                    case _ =>
                }
                signal

            case _ => null
        }
    }

    def canConnectBundledViaInteraction(world:World, pos:BlockPos, side:Direction):Boolean =
        interactions.find(_.isValidInteractionFor(world, pos, side)) match {
            case Some(e) => e.canConnectBundled(world, pos, side)
            case None => false
        }

    def isValidInteractionFor(world:World, pos:BlockPos, side:Direction):Boolean =
        interactions.exists(_.isValidInteractionFor(world, pos, side))

    def getBundledSignalViaInteraction(world:World, pos:BlockPos, side:Direction):Array[Byte] =
        interactions.find(_.isValidInteractionFor(world, pos, side)) match {
            case Some(e) => e.getBundledSignal(world, pos, side)
            case None => null
        }
}
