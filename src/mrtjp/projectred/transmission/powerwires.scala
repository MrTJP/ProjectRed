package mrtjp.projectred.transmission

import java.text.DecimalFormat

import codechicken.lib.vec.Rotation
import codechicken.multipart.TMultiPart
import mrtjp.core.world.{Messenger, WorldLib}
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.nbt.NBTTagCompound

trait TPowerWireCommons extends TWireCommons with IPowerConnectable
{
    val cond:PowerConductor

    override def debug(player:EntityPlayer) = false

    override def test(player:EntityPlayer):Boolean =
    {
        if (world.isRemote) return true

        val p = Messenger.createPacket
        p.writeDouble(x).writeDouble(y).writeDouble(z)
        var s = "/#f"+"#VV\n"+"#IA\n"+"#PW"
        val d = new DecimalFormat("00.00")
        s = s.replace("#V", d.format(cond.voltage()))
        s = s.replace("#I", d.format(cond.amperage))
        s = s.replace("#P", d.format(cond.wattage))
        p.writeString(s)
        p.sendToPlayer(player)
        true
    }

    override def updateAndPropagate(prev:TMultiPart, mode:Int) {}

    override def conductor(side:Int) = cond

    override def conductorOut(id:Int):PowerConductor

    override def doesTick = true

    override def save(tag: NBTTagCompound)
    {
        super.save(tag)
        cond.save(tag)
    }

    override def load(tag: NBTTagCompound)
    {
        super.load(tag)
        cond.load(tag)
    }

    override def update()
    {
        super.update()
        if (!world.isRemote) cond.update()
    }

    override def canConnectPart(part:IConnectable, dir:Int) = part match
    {
        case w:IPowerConnectable => true
        case _ => false
    }
}

abstract class PowerWire extends WirePart with TPowerWireCommons
{
    override def conductorOut(id:Int):PowerConductor =
    {
        if (0 to 3 contains id)
        {
            if (!maskConnects(id)) return null
            if ((connMap&1<<id) != 0) getCorner(id) match //corner
            {
                case p:IPowerConnectable => return p.conductor(rotFromCorner(id))
                case _ => WorldLib.getTileEntity(world, posOfCorner(id), classOf[IPowerConnectable]) match
                {
                    case p:IPowerConnectable if outsideCornerEdgeOpen(id) => return p.conductor(absoluteDir(rotFromCorner(id)))
                    case _ =>
                }
            }
            else if ((connMap&0x10<<id) != 0) getStraight(id) match //straight
            {
                case p:IPowerConnectable => return p.conductor(rotFromStraight(id))
                case _ => WorldLib.getTileEntity(world, posOfStraight(id), classOf[IPowerConnectable]) match
                {
                    case p:IPowerConnectable => return p.conductor(absoluteDir(rotFromStraight(id)))
                    case _ =>
                }
            }
            else if ((connMap&0x100<<id) != 0) getInternal(id) match //internal face
            {
                case p:IPowerConnectable => return p.conductor(id)
                case _ =>
            }
        }
        else if (id == 4) getCenter match
        {
            case p:IPowerConnectable => return p.conductor(side)
            case _ =>
        }

        null
    }

    override def discoverStraightOverride(absDir:Int) = WorldLib.getTileEntity(world, posOfStraight(absDir), classOf[IPowerConnectable]) match
    {
        case p:IPowerConnectable => p.connectStraight(this, absDir^1, Rotation.rotationTo(absDir, side))
        case _ => false
    }

    override def discoverCornerOverride(absDir:Int) = WorldLib.getTileEntity(world, posOfCorner(absoluteRot(absDir)), classOf[IPowerConnectable]) match
    {
        case p:IPowerConnectable => p.connectCorner(this, side^1, Rotation.rotationTo(side, absDir^1))
        case _ => false
    }
}

abstract class FramedPowerWire extends FramedWirePart with TPowerWireCommons
{
    override def conductorOut(id:Int):PowerConductor =
    {
        if (0 until 6 contains id)
        {
            if ((connMap&1<<id) != 0) getStraight(id) match //straight
            {
                case p:IPowerConnectable => return p.conductor(id^1)
                case _ => WorldLib.getTileEntity(world, posOfStraight(id), classOf[IPowerConnectable]) match
                {
                    case p:IPowerConnectable => return p.conductor(id^1)
                    case _ =>
                }
            }
            else if ((connMap&1<<id+6) != 0) getInternal(id) match //internal
            {
                case p:IPowerConnectable => return p.conductor(id^1)
                case _ =>
            }
        }
        null
    }

    override def discoverStraightOverride(s:Int) = WorldLib.getTileEntity(world, posOfStraight(s), classOf[IPowerConnectable]) match
    {
        case p:IPowerConnectable => p.connectStraight(this, s^1, -1)
        case _ => false
    }
}

trait TPW100vCommons extends TPowerWireCommons
{
    protected def connIDRange:Seq[Int]

    val cond = new PowerConductor(this, connIDRange)
    {
        override def capacitance = 8.0D
    }

    def getWireType = WireDef.POWER_100v
}

class PowerWire100v extends PowerWire with TPW100vCommons
{
    override protected def connIDRange = 0 until 5
}

class FramedPowerWire100v extends FramedPowerWire with TPW100vCommons
{
    override protected def connIDRange = 0 until 6
}