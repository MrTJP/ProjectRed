package mrtjp.projectred.transportation

import mrtjp.projectred.api.IConnectable

trait TPressureSubsystem extends PayloadPipePart
{
    override def canConnectPart(part:IConnectable, s:Int) =
        part.isInstanceOf[TPressureSubsystem]
}

trait TPressureTube extends TPressureSubsystem
{

    override def centerReached(r:PipePayload)
    {
        if (Integer.bitCount(connMap) > 2)
        {
            val dim = ~(1<<r.input.ordinal())
            val flags = r.priorityIndex match
            {
                case PressurePriority.inventory => r.priorityIndex
            }
        }

    }
}

trait TPressureDevice extends TPressureSubsystem
{

}

class PressureTube extends BasicPipeAbstraction with TPressureTube
{
}