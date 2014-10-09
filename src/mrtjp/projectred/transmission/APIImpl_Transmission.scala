package mrtjp.projectred.transmission

import codechicken.lib.vec.{BlockCoord, Rotation}
import codechicken.multipart.TileMultipart
import mrtjp.projectred.api.{IBundledEmitter, IBundledTile, IBundledTileInteraction, ITransmissionAPI}
import mrtjp.projectred.core.libmc.PRLib
import net.minecraft.world.World

class APIImpl_Transmission extends ITransmissionAPI
{
    override def getBundledInput(world:World, x:Int, y:Int, z:Int, side:Int):Array[Byte] =
    {
        val pos = new BlockCoord(x, y, z).offset(side)
        world.getTileEntity(pos.x, pos.y, pos.z) match
        {
            case ibt:IBundledTile => ibt.getBundledSignal(side^1)

            case tmp:TileMultipart =>
                var signal:Array[Byte] = null
                def raise(ibe:IBundledEmitter, r:Int)
                {
                    signal = BundledCommons.raiseSignal(signal, ibe.getBundledSignal(r))
                }

                for (r <- 0 until 4)
                {
                    val pside = Rotation.rotateSide(side, r)
                    tmp.partMap(pside) match
                    {
                        case ibe:IBundledEmitter => raise(ibe, Rotation.rotationTo(pside, side^1))
                        case _ =>
                    }
                }
                tmp.partMap(6) match
                {
                    case ibe:IBundledEmitter => raise(ibe, side^1)
                    case _ =>
                }
                signal

            case _ => null
        }
    }

    override def registerBundledTileInteraction(interaction:IBundledTileInteraction)
    {
        APIImpl_Transmission.interactions :+= interaction
    }

    override def containsFramedWire(world:World, x:Int, y:Int, z:Int) =
        PRLib.getMultiPart(world, x, y, z, 6).isInstanceOf[FramedWirePart]

    override def getFramedWireConnectionMask(world:World, x:Int, y:Int, z:Int) =
        PRLib.getMultiPart(world, x, y, z, 6) match
        {
            case f:FramedWirePart => f.clientConnMap
            case _ => -1
        }
}

object APIImpl_Transmission
{
    var interactions = Vector[IBundledTileInteraction]()

    def isValidInteractionFor(world:World, x:Int, y:Int, z:Int) =
        interactions.exists(_.isValidInteractionFor(world, x, y, z))

    def canConnectBundled(world:World, pos:BlockCoord, side:Int):Boolean =
        canConnectBundled(world, pos.x, pos.y, pos.z, side)

    def canConnectBundled(world:World, x:Int, y:Int, z:Int, side:Int):Boolean =
    {
        interactions.find(_.isValidInteractionFor(world, x, y, z)) match
        {
            case Some(e) => e.canConnectBundled(world, x, y, z, side)
            case None => false
        }
    }

    def getBundledSignal(world:World, pos:BlockCoord, side:Int):Array[Byte] =
        getBundledSignal(world, pos.x, pos.y, pos.z, side)

    def getBundledSignal(world:World, x:Int, y:Int, z:Int, side:Int):Array[Byte] =
    {
        interactions.find(_.isValidInteractionFor(world, x, y, z)) match
        {
            case Some(e) => e.getBundledSignal(world, x, y, z, side)
            case None => null
        }
    }
}
