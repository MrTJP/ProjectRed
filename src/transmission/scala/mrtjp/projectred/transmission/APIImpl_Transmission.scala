package mrtjp.projectred.transmission

import codechicken.lib.vec.Rotation
import codechicken.multipart.block.{BlockMultiPart, TileMultiPart}
import mrtjp.projectred.api.{IBundledEmitter, IBundledTile, IBundledTileInteraction, ITransmissionAPI}
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

class APIImpl_Transmission extends ITransmissionAPI
{
    override def registerBundledTileInteraction(interaction:IBundledTileInteraction)
    {
        APIImpl_Transmission.interactions :+= interaction
    }

    override def getBundledInput(world:World, pos:BlockPos, facing:Direction):Array[Byte] =
    {
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

    override def containsBundledCable(world:World, pos:BlockPos, side:Direction) =
        BlockMultiPart.getPart(world, pos, side.ordinal()) match {
            case be:IBundledCablePart => true
            case _ => false
        }

    override def containsFramedWire(world:World, pos:BlockPos) =
        BlockMultiPart.getPart(world, pos, 6).isInstanceOf[FramedWirePart]

    override def getFramedWireConnectionMask(world:World, pos:BlockPos) =
        BlockMultiPart.getPart(world, pos, 6) match {
            case f:FramedWirePart => f.clientConnMap
            case _ => -1
        }
}

object APIImpl_Transmission
{
    var interactions = Vector[IBundledTileInteraction]()

    def isValidInteractionFor(world:World, pos:BlockPos, side:Direction) =
        interactions.exists(_.isValidInteractionFor(world, pos, side))

    def canConnectBundled(world:World, pos:BlockPos, side:Direction):Boolean =
        interactions.find(_.isValidInteractionFor(world, pos, side)) match {
            case Some(e) => e.canConnectBundled(world, pos, side)
            case None => false
        }

    def getBundledSignal(world:World, pos:BlockPos, side:Direction):Array[Byte] =
        interactions.find(_.isValidInteractionFor(world, pos, side)) match {
            case Some(e) => e.getBundledSignal(world, pos, side)
            case None => null
        }
}
