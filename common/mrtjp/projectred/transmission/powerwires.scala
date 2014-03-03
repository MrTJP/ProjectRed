package mrtjp.projectred.transmission

import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.{TPowerConnectable, PowerConductor}

class PowerWire_100v extends PowerWire
{
    val cond = new PowerConductor(this, 0 until 5)
    {
        override def capacitance = 8.0D
    }

    def getWireType = WireDef.POWER_100v

    override def canConnectToType(part:IConnectable) =
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

    override def canConnectToType(part:IConnectable) =
        part.isInstanceOf[PowerWire_100v] ||
            part.isInstanceOf[FramedPowerWire_100v] ||
            (!part.isInstanceOf[WirePart] && part.isInstanceOf[TPowerConnectable])
}