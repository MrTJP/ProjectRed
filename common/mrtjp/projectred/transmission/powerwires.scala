package mrtjp.projectred.transmission

import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.{BlockCoord, Rotation}
import codechicken.multipart.TMultiPart
import java.text.DecimalFormat
import mrtjp.projectred.core._
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.nbt.NBTTagCompound
import mrtjp.projectred.api.IConnectable

abstract class PowerWire extends WirePart with TPowerConnectable
{
    val cond:PowerConductor

    override def debug(player:EntityPlayer) = false

    override def test(player:EntityPlayer):Boolean =
    {
        if (world.isRemote) return true

        val p = new PacketCustom(CoreCPH.channel, CoreCPH.messagePacket)
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
    override def conductorOut(id:Int):PowerConductor =
    {
        if (0 to 3 contains id)
        {
            if (!maskConnects(id)) return null
            if ((connMap&1<<id) != 0) //Corner
            {
                val t = WireConnLib.getCorner(world, side, id, new BlockCoord(tile))
                if (t != null && t.isInstanceOf[TPowerConnectable]) return t.asInstanceOf[TPowerConnectable].conductor(id)

                var t2:TPowerConnectable = null
                val absDir = Rotation.rotateSide(side, id)
                val pos = new BlockCoord(tile)
                pos.offset(absDir)
                if (WireConnLib.canConnectThroughCorner(world, pos, absDir^1, side))
                {
                    pos.offset(side)
                    t2 = BasicUtils.getTileEntity(world, pos, classOf[TPowerConnectable])
                }
                if (t2 != null) return t2.conductor(absDir^1)
                else return null
            }
            else if ((connMap&0x10<<id) != 0) //Straight
            {
                val t = WireConnLib.getStraight(world, side, id, new BlockCoord(tile))
                val absDir = Rotation.rotateSide(side, id)
                if (t != null && t.isInstanceOf[TPowerConnectable]) return t.asInstanceOf[TPowerConnectable].conductor(absDir^1)

                val pos = new BlockCoord(tile).offset(absDir)
                val t2 = BasicUtils.getTileEntity(world, pos, classOf[TPowerConnectable])
                if (t != null) return t2.conductor(absDir^1)
                else return null
            }
            else if ((connMap&0x100<<id) != 0) // internal face
            {
                val t = WireConnLib.getInsideFace(world, side, id, new BlockCoord(tile))
                if (t != null && t.isInstanceOf[TPowerConnectable]) return t.asInstanceOf[TPowerConnectable].conductor(id)
                else return null
            }
        }
        else if (id == 4) // center
        {
            val t = WireConnLib.getCenter(world, new BlockCoord(tile))
            if (t != null && t.isInstanceOf[TPowerConnectable]) return t.asInstanceOf[TPowerConnectable].conductor(side)
            else return null
        }

        null
    }

    override def doesTick = true

    override def discoverStraightOverride(absDir:Int):Boolean =
    {
        val pos = new BlockCoord(tile).offset(absDir)
        val t = BasicUtils.getTileEntity(world, pos, classOf[TPowerConnectable])
        if (t != null) return t.connectStraight(this, absDir^1, Rotation.rotationTo(absDir, side))

        false
    }

    override def discoverCornerOverride(absDir:Int):Boolean =
    {
        val pos = new BlockCoord(tile)
        pos.offset(absDir)
        if (!WireConnLib.canConnectThroughCorner(world, pos, absDir^1, side)) return false
        pos.offset(side)
        val t = BasicUtils.getTileEntity(world, pos, classOf[TPowerConnectable])
        if (t != null) return t.connectCorner(this, side^1, Rotation.rotationTo(side, absDir^1))

        false
    }

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
}

abstract class FramedPowerWire extends FramedWirePart with TPowerConnectable
{
    val cond:PowerConductor

    override def updateAndPropagate(prev:TMultiPart, mode:Int) {}

    override def test(player:EntityPlayer):Boolean =
    {
        if (world.isRemote) return true

        val p = new PacketCustom(CoreCPH.channel, CoreCPH.messagePacket)
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

    override def conductor(id:Int) = cond
    override def conductorOut(id:Int):PowerConductor =
    {
        if (0 until 6 contains id)
        {
            if ((connMap&1<<id) != 0)//straight
            {
                val pos = new BlockCoord(tile).offset(id)
                val t = BasicUtils.getMultipartTile(world, pos)
                if (t != null)
                {
                    val tp = t.partMap(6)
                    if (tp != null && tp.isInstanceOf[TPowerConnectable]) return tp.asInstanceOf[TPowerConnectable].conductor(id^1)
                }
                val t2 = BasicUtils.getTileEntity(world, pos, classOf[TPowerConnectable])
                if (t2 != null) t2.conductor(id^1)
                else return null
            }
            else if ((connMap&1<<id+6) != 0)//internal
            {
                val t = tile.partMap(id)
                if (t != null && t.isInstanceOf[TPowerConnectable]) return t.asInstanceOf[TPowerConnectable].conductor(id^1)
            }
        }
        null
    }

    override def doesTick = true

    override def discoverStraightOverride(s:Int):Boolean =
    {
        val pos = new BlockCoord(tile).offset(s)
        val tp = BasicUtils.getTileEntity(world, pos, classOf[TPowerConnectable])
        if (tp != null) return tp.connectStraight(this, s^1, -1)
        false
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        cond.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        cond.load(tag)
    }

    override def update()
    {
        super.update()
        if (!world.isRemote) cond.update()
    }
}

class PowerWire_100v extends PowerWire
{
    val cond = new PowerConductor(this, 0 until 5)
    {
        override def capacitance = 8.0D
    }

    def getWireType = WireDef.POWER_100v

    override def canConnectPart(part:IConnectable, r:Int) =
        part.isInstanceOf[PowerWire_100v] ||
            part.isInstanceOf[FramedPowerWire_100v] ||
            (!part.isInstanceOf[WirePart] && part.isInstanceOf[TPowerConnectable])

}

class FramedPowerWire_100v extends FramedPowerWire
{
    val cond = new PowerConductor(this, 0 until 6)
    {
        override def capacitance = 8.0D
    }

    override def getWireType = WireDef.POWER_100v

    override def canConnectPart(part:IConnectable, s:Int) =
        part.isInstanceOf[PowerWire_100v] ||
            part.isInstanceOf[FramedPowerWire_100v] ||
            (!part.isInstanceOf[WirePart] && part.isInstanceOf[TPowerConnectable])
}