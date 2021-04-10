package mrtjp.projectred.transmission

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.Rotation
import codechicken.multipart.api.part.TMultiPart
import mrtjp.core.world.Messenger
import mrtjp.projectred.api.{IBundledEmitter, IBundledTile, IConnectable, IMaskedBundledTile}
import mrtjp.projectred.core.IWirePart._
import mrtjp.projectred.core._
import net.minecraft.entity.player.{PlayerEntity, ServerPlayerEntity}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.Direction
import net.minecraft.util.text.StringTextComponent

trait IBundledCablePart extends IWirePart with IBundledEmitter
{
    def getBundledSignal:Array[Byte]

    def calculateSignal:Array[Byte]

    def setSignal(newSignal:Array[Byte])

    def getBundledColour:Int
}

trait TBundledCableCommons extends TWireCommons with TBundledAquisitionsCommons with IBundledCablePart
{
    var signal = new Array[Byte](16) //server-side only
    var colour = getWireType.getColourIdx.toByte

    override def save(tag:CompoundNBT)
    {
        super.save(tag)
        tag.putByteArray("signal", signal)
        tag.putByte("colour", colour)
    }

    override def load(tag:CompoundNBT)
    {
        super.load(tag)
        signal = tag.getByteArray("signal")
        colour = tag.getByte("colour")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(colour)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        colour = packet.readByte()
    }

    override def canConnectPart(part:IConnectable, dir:Int) = part match
    {
        case b:IBundledCablePart => b.getBundledColour == -1 || colour == -1 || b.getBundledColour == colour
        case ins:IInsulatedRedwirePart => true
        case be:IBundledEmitter => true
        case _ => false
    }

    protected var propagatingMask = 0xFFFF
    override def updateAndPropagate(from:TMultiPart, mode:Int)
    {
        import mrtjp.projectred.core.BundledCommons._
        val mask = getUpdateMask(from, mode)
        if (mode == DROPPING && isSignalZero(getBundledSignal, mask)) return

        val newSignal = calculateSignal
        applyChangeMask(getBundledSignal, newSignal, mask)

        propagatingMask = mask

        if (dropSignalsLessThan(getBundledSignal, newSignal))
        {
            if (!isSignalZero(newSignal, mask)) WirePropagator.propagateAnalogDrop(this)
            propagate(from, DROPPING)
        }
        else if (!signalsEqual(getBundledSignal, newSignal))
        {
            setSignal(newSignal)
            if (mode == DROPPING) propagate(null, RISING)
            else propagate(from, RISING)
        }
        else if (mode == DROPPING) propagateTo(from, RISING)
        else if (mode == FORCE) propagate(from, FORCED)

        propagatingMask = 0xFFFF
    }

    private def getUpdateMask(from:TMultiPart, mode:Int) = from match
    {
        case ins:IInsulatedRedwirePart => 1<<ins.getInsulatedColour
        case b:IBundledCablePart if mode == DROPPING =>
            var m = 0
            val osignal = b.getBundledSignal
            for (i <- 0 until 16) if (osignal(i) == 0) m |= 1<<i
            m
        case b:IBundledCablePart if mode == RISING =>
            var m = 0
            val osignal = b.getBundledSignal
            for (i <- 0 until 16) if ((osignal(i)&0xFF) > (getBundledSignal.apply(i)&0xFF)) m |= 1<<i
            m
        case _ => 0xFFFF
    }

    override def resolveArray(part:Any, r:Int) =
    {
        part match
        {
            case b:IBundledCablePart =>
                val osig = b.getBundledSignal
                for (i <- 0 until 16) if ((osig(i)&0xFF)-1 > (tmpSignal(i)&0xFF))
                    tmpSignal(i) = (osig(i)-1).toByte
            case i:IInsulatedRedwirePart =>
                val s = i.getRedwireSignal(r)-1
                if (s > (tmpSignal(i.getInsulatedColour)&0xFF))
                    tmpSignal(i.getInsulatedColour) = s.toByte
            case b:IBundledEmitter => BundledCommons.raiseSignal(tmpSignal, b.getBundledSignal(r))
            case t:TileEntity => BundledCommons.raiseSignal(tmpSignal,
                BundledSignalsLib.getBundledSignalViaInteraction(t.getWorld, t.getPos, Direction.byIndex(r)))
            case _ =>
        }
        tmpSignal
    }

    protected val tmpSignal = new Array[Byte](16)
    protected def tmpSignalClear()
    {
        for (i <- 0 until 16) tmpSignal(i) = 0.toByte
    }

    override def propagateTo(part:TMultiPart, mode:Int) =
    {
        def shouldPropogate(part:TMultiPart, mode:Int) = part match
        {
            case ins:IInsulatedRedwirePart => (propagatingMask&1<<ins.getInsulatedColour) != 0
            case _ => true
        }

        if (shouldPropogate(part, mode)) super.propagateTo(part, mode)
        else true
    }

    override def setSignal(newSignal:Array[Byte])
    {
        if (newSignal == null) signal.transform(_ => 0.toByte)
        else for (i <- 0 until 16) signal(i) = newSignal(i)
    }

    override def getBundledSignal = signal

    override def getBundledSignal(dir:Int) = if (maskConnects(dir)) getBundledSignal else null

    override def getBundledColour = colour

    override def debug(player:PlayerEntity):Boolean =
    {
        val sb = new StringBuilder
        for (i <- 0 until 16)
        {
            val s = Integer.toHexString(signal(i)&0xFF).toUpperCase
            if (s.length == 1) sb.append('0')
            sb.append(s)
        }
        player.sendMessage(new StringTextComponent(sb.toString()))
        true
    }

    override def test(player: PlayerEntity) =
    {
        if (!world.isRemote && player.isInstanceOf[ServerPlayerEntity]) {
            var s = ""
            for (i <- 0 until 16) if (getBundledSignal.apply(i) != 0) s = s+"["+i+"]"

            if (s == "") s = "off"
            val packet = Messenger.createPacket
            packet.writeDouble(pos.getX + 0.0D)
            packet.writeDouble(pos.getY + 0.5D)
            packet.writeDouble(pos.getZ + 0.0D)
            packet.writeString("/#f"+s)
            packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
        }
        true
    }

    override def useStaticRenderer = true
}

class BundledCablePart(wireType: WireType) extends WirePart(wireType) with TFaceBundledAquisitions with TBundledCableCommons
{
    override def calculateSignal =
    {
        tmpSignalClear()
        for (r <- 0 until 4) if (maskConnects(r)) {
            if (maskConnectsCorner(r)) calcCornerArray(r)
            else {
                if (maskConnectsStraight(r)) calcStraightArray(r)
                calcInternalArray(r)
            }
        }
        if (maskConnectsCenter) calcCenterArray
        tmpSignal
    }

    override def calcStraightArray(r:Int) =
    {
        world.getTileEntity(posOfStraight(r)) match {
            case ibe:IBundledEmitter => resolveArray(ibe, absoluteDir(rotFromStraight(r)))
            case t:TileEntity if BundledSignalsLib.isValidInteractionFor(world, t.getPos, Direction.byIndex(rotFromStraight(r))) =>
                resolveArray(t, absoluteDir(rotFromStraight(r)))
            case _ => super.calcStraightArray(r)
        }
    }

    override def discoverStraightOverride(absDir:Int) =
    {
        val pos = this.pos.offset(Direction.byIndex(absDir))
        world.getTileEntity(pos) match {
            case b:IMaskedBundledTile => b.canConnectBundled(absDir^1) &&
                    (b.getConnectionMask(absDir^1)&1<<Rotation.rotationTo(absDir, side)) != 0
            case b:IBundledTile => b.canConnectBundled(absDir^1)
            case _ => BundledSignalsLib.canConnectBundledViaInteraction(world, pos, Direction.byIndex(absDir^1))
        }
    }
}

class FramedBundledCablePart(wireType: WireType) extends FramedWirePart(wireType) with TCenterBundledAquisitions with TBundledCableCommons
{
    override def calculateSignal =
    {
        tmpSignalClear()
        for (s <- 0 until 6) if (maskConnects(s))
            if (maskConnectsOut(s)) calcStraightArray(s)
            else calcInternalArray(s)

        tmpSignal
    }

    override def calcStraightArray(s:Int) =
    {
        world.getTileEntity(posOfStraight(s)) match {
            case ibe:IBundledEmitter => resolveArray(ibe, s^1)
            case t:TileEntity if BundledSignalsLib.isValidInteractionFor(world, t.getPos, Direction.byIndex(s^1)) =>
                resolveArray(t, s^1)
            case _ => super.calcStraightArray(s)
        }
    }

    override def discoverStraightOverride(absDir:Int) =
    {
        val pos = this.pos.offset(Direction.byIndex(absDir))
        world.getTileEntity(pos) match {
            case b:IMaskedBundledTile => b.canConnectBundled(absDir^1) &&
                    (b.getConnectionMask(absDir^1)&0x10) != 0
            case b:IBundledTile => b.canConnectBundled(absDir^1)
            case _ => BundledSignalsLib.canConnectBundledViaInteraction(world, pos, Direction.byIndex(absDir^1))
        }
    }
}
