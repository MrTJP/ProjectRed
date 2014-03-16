package mrtjp.projectred.transmission

import codechicken.lib.data.{MCDataOutput, MCDataInput}
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.BlockCoord
import codechicken.multipart.TMultiPart
import mrtjp.projectred.api.{IBundledTile, IConnectable, IBundledEmitter}
import mrtjp.projectred.core.{CoreSPH, BasicUtils}
import mrtjp.projectred.transmission.IWirePart._
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.ChatMessageComponent

trait IBundledCablePart extends IWirePart with IBundledEmitter
{
    def getBundledSignal:Array[Byte]

    def calculateSignal:Array[Byte]

    def setSignal(newSignal:Array[Byte])

    def getBundledColour:Int
}

trait TBundledCableCommons extends TWireCommons with TBundledAquisitionsCommons with IBundledCablePart
{
    /**
     * Not available on client
     */
    var signal = new Array[Byte](16)
    var colour:Byte = 0

    def getWireType = WireDef.BUNDLED_WIRE(colour+1)

    override def preparePlacement(side:Int, meta:Int)
    {
        super.preparePlacement(side, meta)
        colour = (meta-WireDef.BUNDLED_0.meta).asInstanceOf[Byte]
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByteArray("signal", signal)
        tag.setByte("colour", colour)
    }

    override def load(tag:NBTTagCompound)
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
        colour = packet.readByte
    }

    override def canConnectPart(part:IConnectable, r:Int) = part match
    {
        case b:IBundledCablePart => b.getBundledColour == -1 || colour == -1 || b.getBundledColour == colour
        case ins:IInsulatedRedwirePart => true
        case be:IBundledEmitter => true
        case _ => false
    }

    override def discoverStraightOverride(absDir:Int) =
    {
        val pos = new BlockCoord(tile).offset(absDir)
        world.getBlockTileEntity(pos.x, pos.y, pos.z) match
        {
            case b:IBundledTile => b.canConnectBundled(absDir^1)
            case _ => false
        }
    }

    protected var propogatingMask = 0xFFFF
    override def updateAndPropagate(from:TMultiPart, mode:Int)
    {
        import BundledCommons._
        val mask = getUpdateMask(from, mode)
        if (mode == DROPPING && isSignalZero(getBundledSignal, mask)) return

        val newSignal = calculateSignal
        applyChangeMask(getBundledSignal, newSignal, mask)

        propogatingMask = mask

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

        propogatingMask = 0xFFFF
    }

    def getUpdateMask(from:TMultiPart, mode:Int) = from match
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

    override def resolveArray(part:TMultiPart, r:Int) =
    {
        raiseSignalFrom(part, r)
        tmpSignal
    }

    abstract override def calcStraightArray(dir:Int) =
    {
        val te = BasicUtils.getTileEntity(world, posOfStraight(dir), classOf[IBundledEmitter])
        if (te != null)
        {
            raiseSignalFrom(te, dir)
            tmpSignal
        }
        else super.calcStraightArray(dir)
    }

    val tmpSignal = new Array[Byte](16)
    def tmpSignalClear()
    {
        tmpSignal.transform(_ => 0.asInstanceOf[Byte])
    }
    def raiseSignalFrom(part:AnyRef, r:Int) = part match
    {
        case b:IBundledCablePart =>
            val osig = b.getBundledSignal
            for (i <- 0 until 16)
                if ((osig(i)&0xFF)-1 > (tmpSignal(i)&0xFF)) tmpSignal(i) = (osig(i)-1).asInstanceOf[Byte]
        case i:IInsulatedRedwirePart =>
            val s = i.getRedwireSignal(r)-1
            if (s > (tmpSignal(i.getInsulatedColour)&0xFF))
                tmpSignal(i.getInsulatedColour) = s.asInstanceOf[Byte]
        case b:IBundledEmitter => BundledCommons.raiseSignal(tmpSignal, b.getBundledSignal(r))
        case _ =>
    }

    override def propagateTo(part:TMultiPart, mode:Int) =
    {
        def shouldPropogate(part:TMultiPart, mode:Int) = part match
        {
            case ins:IInsulatedRedwirePart => (propogatingMask&1<<ins.getInsulatedColour) != 0
            case _ => true
        }

        if (shouldPropogate(part, mode)) super.propagateTo(part, mode)
        else true
    }

    override def setSignal(newSignal:Array[Byte])
    {
        if (newSignal == null) signal.transform(_ => 0.asInstanceOf[Byte])
        else for (i <- 0 until 16) signal(i) = newSignal(i)
    }

    override def getBundledSignal = signal

    override def getBundledSignal(side:Int) = if (maskConnects(side)) getBundledSignal else null

    override def getBundledColour = colour

    override def debug(player:EntityPlayer):Boolean =
    {
        val sb = new StringBuilder
        for (i <- 0 until 16)
        {
            val s = Integer.toHexString(signal(i)&0xFF).toUpperCase
            if (s.length == 1) sb.append('0')
            sb.append(s)
        }
        player.sendChatToPlayer(ChatMessageComponent.createFromTranslationKey(sb.toString()))
        true
    }

    override def test(player:EntityPlayer) =
    {
        if (BasicUtils.isServer(world))
        {
            var s = ""
            for (i <- 0 until 16) if (getBundledSignal.apply(i) != 0) s = s+"["+i+"]"

            if (s == "") s = "off"
            val packet = new PacketCustom(CoreSPH.channel, CoreSPH.messagePacket)
            packet.writeDouble(x + 0.0D)
            packet.writeDouble(y + 0.5D)
            packet.writeDouble(z + 0.0D)
            packet.writeString("/#f"+s)
            packet.sendToPlayer(player)
        }
        true
    }

    override def useStaticRenderer = true
}

class BundledCablePart extends WirePart with TFaceBundledAquisitions with TBundledCableCommons
{
    override def calculateSignal =
    {
        tmpSignalClear()
        for (r <- 0 until 4) if (maskConnects(r))
        {
            if (maskConnectsCorner(r)) calcCornerArray(r)
            else
            {
                if (maskConnectsStraight(r)) calcStraightArray(r)
                calcInternalArray(r)
            }
        }
        if (maskConnectsCenter) calcCenterArray
        tmpSignal
    }
}

class FramedBundledCablePart extends FramedWirePart with TCenterBundledAquisitions with TBundledCableCommons
{
    override def calculateSignal =
    {
        tmpSignalClear()
        for (s <- 0 until 6) if (maskConnects(s))
            if (maskConnectsOut(s)) calcStraightArray(s)
            else calcInternalArray(s)

        tmpSignal
    }
}