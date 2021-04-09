package mrtjp.projectred.transmission

import codechicken.lib.vec.Rotation
import codechicken.multipart.api.part.{ITickablePart, TMultiPart}
import mrtjp.core.world.Messenger
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import net.minecraft.entity.player.{PlayerEntity, ServerPlayerEntity}
import net.minecraft.nbt.CompoundNBT

import java.text.DecimalFormat

trait TPowerWireCommons extends TWireCommons with TPowerPartCommons with ITickablePart
{
    val cond:PowerConductor

    override def save(tag: CompoundNBT)
    {
        super.save(tag)
        cond.save(tag)
    }

    override def load(tag: CompoundNBT)
    {
        super.load(tag)
        cond.load(tag)
    }

    override def conductor(side:Int) = cond

    def getExternalCond(id:Int):PowerConductor

    override def updateAndPropagate(prev:TMultiPart, mode:Int) {}

    override def tick()
    {
        if (!world.isRemote) cond.update()
    }

    override def test(player:PlayerEntity):Boolean =
    {
        if (world.isRemote && !player.isInstanceOf[ServerPlayerEntity]) return true

        val p = Messenger.createPacket
        p.writeDouble(pos.getX).writeDouble(pos.getY).writeDouble(pos.getZ)
        var s = "/#f"+"#VV\n"+"#IA\n"+"#PW"
        val d = new DecimalFormat("00.00")
        s = s.replace("#V", d.format(cond.voltage()))
        s = s.replace("#I", d.format(cond.current))
        s = s.replace("#P", d.format(cond.power))
        p.writeString(s)
        p.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])

        true
    }

    override def canConnectPart(part:IConnectable, dir:Int) = part match
    {
        case w:IPowerConnectable => true
        case _ => false
    }
}

trait PowerWire extends WirePart with TPowerWireCommons with TFacePowerPart
{
    override def discoverStraightOverride(absDir:Int) = world.getTileEntity(posOfStraight(absoluteRot(absDir))) match
    {
        case p:IPowerConnectable => p.connectStraight(this, absDir^1, Rotation.rotationTo(absDir, side))
        case _ => false
    }

    override def discoverCornerOverride(absDir:Int) = world.getTileEntity(posOfCorner(absoluteRot(absDir))) match
    {
        case p:IPowerConnectable =>
            p.connectCorner(this, side^1, Rotation.rotationTo(side, absDir^1))
        case _ => false
    }
}

trait FramedPowerWire extends FramedWirePart with TPowerWireCommons with TCenterPowerPart
{
    override def discoverStraightOverride(s:Int) = world.getTileEntity(posOfStraight(s)) match
    {
        case p:IPowerConnectable =>
            p.connectStraight(this, s^1, -1)
        case _ => false
    }
}

trait TLowLoadPowerLineCommons extends TPowerWireCommons with ILowLoadPowerLine
{
    val cond = new PowerConductor(this, idRange)
    {
        override def capacitance = 8.0D
        override def resistance = 0.01D
        override def scaleOfInductance = 0.07D
        override def scaleOfParallelFlow = 0.5D
    }

    override def connWorld = world
}

class LowLoadPowerLine(wireType:WireType) extends WirePart(wireType) with PowerWire with TLowLoadPowerLineCommons
class FramedLowLoadPowerLine(wireType:WireType) extends FramedWirePart(wireType) with FramedPowerWire with TLowLoadPowerLineCommons
