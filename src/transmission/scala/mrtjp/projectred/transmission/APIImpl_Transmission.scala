package mrtjp.projectred.transmission

import codechicken.lib.vec.Rotation
import codechicken.multipart.block.{BlockMultiPart, TileMultiPart}
import mrtjp.projectred.api.{IBundledEmitter, IBundledTile, IBundledTileInteraction, ITransmissionAPI}
import mrtjp.projectred.core.{BundledCommons, BundledSignalsLib}
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

object APIImpl_Transmission extends ITransmissionAPI
{
    override def registerBundledTileInteraction(interaction:IBundledTileInteraction):Unit = {
        BundledSignalsLib.registerBundledTileInteraction(interaction)
    }

    override def getBundledInput(world:World, pos:BlockPos, facing:Direction):Array[Byte] = {
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

    override def containsBundledCable(world:World, pos:BlockPos, side:Direction):Boolean =
        BlockMultiPart.getPart(world, pos, side.ordinal()) match {
            case be:IBundledCablePart => true
            case _ => false
        }

    override def containsFramedWire(world:World, pos:BlockPos):Boolean =
        BlockMultiPart.getPart(world, pos, 6).isInstanceOf[FramedWirePart]

    override def getFramedWireConnectionMask(world:World, pos:BlockPos):Int =
        BlockMultiPart.getPart(world, pos, 6) match {
            case f:FramedWirePart => f.clientConnMap
            case _ => -1
        }
}
