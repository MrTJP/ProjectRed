package mrtjp.projectred.transmission

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.Rotation
import codechicken.multipart.`trait`.TRedstoneTile
import codechicken.multipart.`trait`.extern.IRedstoneTile
import codechicken.multipart.api.part.{RedstoneInteractions, TMultiPart}
import codechicken.multipart.api.part.redstone.{IFaceRedstonePart, IMaskedRedstonePart, IRedstonePart}
import mrtjp.core.world.Messenger
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import net.minecraft.entity.player.{PlayerEntity, ServerPlayerEntity}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.{ActionResultType, Direction}
import net.minecraft.util.text.StringTextComponent
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

trait TRedwireCommons extends TWireCommons with TRSAcquisitionsCommons with TRSPropagationCommons with IRedwirePart
{
    var signal:Byte = 0

    override def save(tag:CompoundNBT)
    {
        super.save(tag)
        tag.putByte("signal", signal)
    }

    override def load(tag:CompoundNBT)
    {
        super.load(tag)
        signal = tag.getByte("signal")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(signal)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        signal = packet.readByte
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 10 =>
            signal = packet.readByte()
            if (Configurator.staticWires) tile.markRender()
        case _ => super.read(packet, key)
    }

    override def strongPowerLevel(side:Int) = 0

    override def weakPowerLevel(side:Int) = 0

    def rsLevel =
    {
        if (WirePropagator.redwiresProvidePower) ((signal&0xFF)+16)/17
        else 0
    }

    override def getRedwireSignal(side:Int) = getSignal

    override def getSignal = signal&0xFF
    override def setSignal(sig:Int){ signal = sig.toByte }

    override def onSignalUpdate()
    {
        super.onSignalUpdate()
        sendUpdate(10, _.writeByte(signal))
    }

    override def debug(player:PlayerEntity) =
    {
        player.sendMessage(new StringTextComponent(
            (if (world.isRemote) "Client" else "Server")+" signal strength: "+getSignal))
        true
    }

    override def test(player:PlayerEntity) =
    {
        if (world.isRemote) Messenger.addMessage(pos.getX, pos.getY+0.5, pos.getZ, "/#f/#c[c] = "+getSignal)
        else if (player.isInstanceOf[ServerPlayerEntity]) {
            val packet = Messenger.createPacket
            packet.writeDouble(pos.getX+0.0D)
            packet.writeDouble(pos.getY+0.5D)
            packet.writeDouble(pos.getZ+0.0D)
            packet.writeString("/#c[s] = "+getSignal)
            packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
        }
        true
    }
}

abstract class RedwirePart(wireType:WireType) extends WirePart(wireType) with TRedwireCommons with TFaceRSAcquisitions with TFaceRSPropagation
{
    override def weakPowerLevel(side:Int) =
    {
        if ((side&6) != (this.side&6) && (connMap&0x100<<Rotation.rotationTo(this.side, side)) != 0) 0
        else rsLevel
    }

    def canConnectRedstone(side:Int) = WirePropagator.redwiresConnectable

    override def discoverOpen(r:Int) =
    {
        val absDir = absoluteDir(r)
        (tile.asInstanceOf[IRedstoneTile].openConnections(absDir)&1<<Rotation.rotationTo(absDir&6, side)) != 0
    }

    override def canConnectPart(wire:IConnectable, r:Int) =
        wire.isInstanceOf[IRedwireEmitter] || wire.isInstanceOf[IRedstonePart]

    override def discoverStraightOverride(absDir:Int) =
    {
        val conn = WirePropagator.redwiresConnectable
        WirePropagator.setRedwiresConnectable(true)
        val disc = (RedstoneInteractions.otherConnectionMask(world, pos, absDir, false)&
            RedstoneInteractions.connectionMask(this, absDir)) != 0
        WirePropagator.setRedwiresConnectable(conn)
        disc
    }

    override def discoverInternalOverride(p:TMultiPart, r:Int) = p match
    {
        case rsp:IFaceRedstonePart => rsp.canConnectRedstone(side)
        case _ => false
    }

    override def calcStraightSignal(r:Int) =
    {
        val partsig = super.calcStraightSignal(r)
        if (partsig > 0) partsig
        else calcMaxSignal(r, true, true)
    }

    override def resolveSignal(part:Any, r:Int) = part match
    {
        case t:IRedwirePart if t.diminishOnSide(r) => t.getRedwireSignal(r)-1
        case t:IRedwireEmitter => t.getRedwireSignal(r)
        case t:IFaceRedstonePart =>
            val s = Rotation.rotateSide(t.getFace, r)
            Math.max(t.strongPowerLevel(s), t.weakPowerLevel(s))*17
        case _ => 0
    }

    override def calculateSignal =
    {
        WirePropagator.setDustProvidePower(false)
        WirePropagator.redwiresProvidePower = false
        var s = 0
        def raise(sig:Int){ if (sig > s) s = sig }

        for (r <- 0 until 4) if (maskConnects(r))
            if (maskConnectsCorner(r)) raise(calcCornerSignal(r))
            else
            {
                if (maskConnectsStraight(r)) raise(calcStraightSignal(r))
                raise(calcInternalSignal(r)) //TODO else?
            }

        raise(calcUndersideSignal)
        if (maskConnectsCenter) raise(calcCenterSignal)

        WirePropagator.setDustProvidePower(true)
        WirePropagator.redwiresProvidePower = true
        s
    }
}

abstract class FramedRedwirePart(wireType:WireType) extends FramedWirePart(wireType) with TRedwireCommons with TCenterRSAcquisitions with TCenterRSPropagation with IMaskedRedstonePart
{
    override def weakPowerLevel(side:Int) = rsLevel

    override def canConnectRedstone(side:Int) = true

    override def getConnectionMask(side:Int) = 0x10

    override def canConnectPart(part:IConnectable, s:Int) = part.isInstanceOf[IRedwirePart]

    override def discoverStraightOverride(absDir:Int) =
    {
        WirePropagator.setRedwiresConnectable(false)
        val b = (RedstoneInteractions.otherConnectionMask(world, pos, absDir, false) &
                RedstoneInteractions.connectionMask(this, absDir)) != 0
        WirePropagator.setRedwiresConnectable(true)
        b
    }

    override def discoverInternalOverride(p:TMultiPart, s:Int) = p match
    {
        case rsPart:IRedstonePart => rsPart.canConnectRedstone(s^1)
        case _ => false
    }

    override def propagateOther(mode:Int)
    {
        for (s <- 0 until 6) if (!maskConnects(s))
            WirePropagator.addNeighborChange(pos.offset(Direction.byIndex(s)))
    }

    def calculateSignal =
    {
        WirePropagator.setDustProvidePower(false)
        WirePropagator.redwiresProvidePower = false
        var s = 0
        def raise(sig:Int) {if (sig > s) s = sig}

        for (s <- 0 until 6)
        {
            if (maskConnectsIn(s)) raise(calcInternalSignal(s))
            else if (maskConnectsOut(s)) raise(calcStraightSignal(s))
        }

        WirePropagator.setDustProvidePower(true)
        WirePropagator.redwiresProvidePower = true
        s
    }

    override def calcStraightSignal(s:Int) = getStraight(s) match
    {
        case p:TMultiPart => resolveSignal(p, s^1)
        case _ => calcStrongSignal(s)
    }

    override def calcInternalSignal(s:Int) =
    {
        val tp = getInternal(s)
        val sig = resolveSignal(tp, s^1)
        if (sig > 0) sig
        else tp match
        {
            case rp:IRedstonePart => Math.max(rp.strongPowerLevel(s^1), rp.weakPowerLevel(s^1))<<4
            case _ => 0
        }
    }

    override def resolveSignal(part:Any, s:Int) = part match
    {
        case rw:IRedwirePart if rw.diminishOnSide(s) => rw.getRedwireSignal(s)-1
        case re:IRedwireEmitter => re.getRedwireSignal(s)
        case _ => 0
    }
}

trait TRedAlloyCommons extends TRedwireCommons
{
    override def renderHue = (signal&0xFF)/2+60<<24|0xFF
}

class RedAlloyWirePart(wireType:WireType) extends RedwirePart(wireType) with TRedAlloyCommons
{
    override def strongPowerLevel(side:Int) = if (side == this.side) rsLevel else 0

    override def redstoneConductionMap = 0x1F

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote) tile.notifyNeighborChange(side)
    }

    override def propagateOther(mode:Int)
    {
        WirePropagator.addNeighborChange(pos.offset(Direction.byIndex(side)))
        WirePropagator.addNeighborChange(pos.offset(Direction.byIndex(side^1)))

        for (r <- 0 until 4) if (!maskConnects(r))
            WirePropagator.addNeighborChange(pos.offset(Direction.byIndex(Rotation.rotateSide(side, r))))

        for (s <- 0 until 6) if (s != (side^1))
            WirePropagator.addNeighborChange(pos
                    .offset(Direction.byIndex(side)).offset(Direction.byIndex(s)))
    }
}

class FramedRedAlloyWirePart(wireType:WireType) extends FramedRedwirePart(wireType) with TRedAlloyCommons

trait TInsulatedCommons extends TRedwireCommons with IInsulatedRedwirePart
{
    var colour = getWireType.getColourIdx.toByte

    override def save(tag:CompoundNBT)
    {
        super.save(tag)
        tag.putByte("colour", colour)
    }

    override def load(tag:CompoundNBT)
    {
        super.load(tag)
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

    abstract override def resolveSignal(part:Any, dir:Int) = part match
    {
        case b:IBundledCablePart => (b.getBundledSignal.apply(colour)&0xFF)-1
        case _ => super.resolveSignal(part, dir)
    }

    abstract override def canConnectPart(part:IConnectable, r:Int) = part match
    {
        case b:IBundledCablePart => true
        case w:IInsulatedRedwirePart => w.getInsulatedColour == colour
        case _ => super.canConnectPart(part, r)
    }

    @OnlyIn(Dist.CLIENT)
    override def getIcon = getWireType.getTextures.get(if (signal != 0) 1 else 0)

    override def getInsulatedColour = colour
}

class InsulatedRedAlloyPart(wireType:WireType) extends RedwirePart(wireType) with TInsulatedCommons
{
    override def weakPowerLevel(side:Int) =
    {
        if (this.side == side || this.side == (side^1) || !maskConnects(absoluteRot(side))) 0
        else super.weakPowerLevel(side)
    }

    override def calcUndersideSignal = 0
}

class FramedInsulatedRedAlloyPart(wireType:WireType) extends FramedRedwirePart(wireType) with TInsulatedCommons
{
    override def weakPowerLevel(side:Int) =
    {
        if (!maskConnects(side)) 0
        else super.weakPowerLevel(side)
    }
}
