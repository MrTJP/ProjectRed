package mrtjp.projectred.core

import mrtjp.projectred.api.IConnectable
import net.minecraft.nbt.CompoundNBT
import net.minecraft.world.World

import java.util
import scala.collection.mutable.{Set => MSet}
import scala.jdk.CollectionConverters._

/**
 * Interface for things that wish to conduct/use electricity.
 */
trait IPowerConnectable extends IConnectable
{
    /**
     * Getter for the local conductor
     * @param dir Side of the required conductor, this
     *             is only used if the tile has multiple
     *             linked conductors (such as voltage transformers).
     *             Rotation for face parts, absDir else.
     * @return The local conductor managed by this object.
     */
    def conductor(dir:Int):PowerConductor

    /**
     * This should reach out and grab a conductor from another
     * TPowerConnectable through the method above.
     * @param id Each conductor this can possibly grab should have
     *           a designated ID. This is used internally. Calling
     *           this method with the same ID should yield the
     *           same neighbor conductor.
     * @return The neighbor conductor that corresponds with the id.
     *         Can be NULL if that ID is not connected, etc.
     */
    def conductorOut(id:Int):PowerConductor

    /**
     * Reference to the world. This is used for checking world time
     * to accurately calculate/distribute current.
     * @return The world this TPowerConnectable is in. Should be not null.
     */
    def connWorld:World
}

/**
 * Object held by conducting power tiles, self managed through
 * TPowerConnectable. This model of electrical flow loosely emulates
 * phisics of real world electrical flow in a series circuit.
 * @param parent The "actual" conductor (as in, the tile).
 * @param ids The possible connections to other conductors
 *            this can make.
 */
class PowerConductor(val parent:IPowerConnectable, ids:Seq[Int])
{
    val flows = new Array[Double](ids.max+1)

    var Vloc = 0.0D //local electric potential
    var Iloc = 0.0D //local electric current

    var Vflow = 0.0D //aquired uncalculated voltage
    var Iflow = 0.0D //aquired uncalculated current

    var time = 0

    def isValid = parent.connWorld != null

    def capacitance = 0.0D
    def resistance = 0.01D
    def scaleOfInductance = 0.07D
    def scaleOfParallelFlow = 0.5D

    /**
     * Re-calculates V and I if needed.
     * @return The electric potential, in Volts (V)
     */
    def voltage() =
    {
        val tick = parent.connWorld.getGameTime
        if ((tick & 0xFFFF) != time) {
            time = (tick & 0xFFFF).asInstanceOf[Int]
            //calculate voltage
            Iloc = 0.5D * Iflow
            Iflow = 0.0D
            Vloc += 0.05D * Vflow * capacitance
            Vflow = 0.0D
        }
        Vloc
    }

    /**
     * @return The current(I), in Amps (A)
     */
    def current =
    {
        voltage()
        Iloc
    }

    /**
     * @return The power(P), in Watts (W)
     */
    def power = voltage()*Iloc

    def applyCurrent(I:Double)
    {
        voltage()
        Vflow += I
        Iflow += math.abs(I)
    }

    def applyPower(P:Double)
    {
        val Ptot = voltage()*Vloc + 0.1D*P*capacitance
        val dP = math.sqrt(Ptot)-Vloc
        applyCurrent(20.0D*dP/capacitance)
    }

    def drawPower(P:Double)
    {
        val Ptot = voltage()*Vloc - 0.1D*P*capacitance
        val dP = if (Ptot < 0.0D) 0.0D else math.sqrt(Ptot)-Vloc
        applyCurrent(20.0D*dP/capacitance)
    }

    def powerTotal = (voltage()*Vloc)/(0.1D*capacitance)

    def update()
    {
        voltage()
        for (id <- ids)
            if (!surge(parent.conductorOut(id), id)) flows(id) = 0.0D

        surgeIn.clear()
    }

    def surge(cond:PowerConductor, id:Int) =
    {
        if (cond == null) false
        else if (cond.parent == parent) false
        else if (surgeIn.contains(cond)) true
        else
        {
            val r = resistance+cond.resistance
            var I = flows(id)
            val V = Vloc-cond.voltage()
            flows(id) += (V-I*r)*scaleOfInductance
            I += V*scaleOfParallelFlow

            applyCurrent(-I)
            cond.applySurge(this, I)

            true
        }
    }

    var surgeIn = MSet[PowerConductor]()
    def applySurge(from:PowerConductor, Iin:Double)
    {
        surgeIn += from
        applyCurrent(Iin)
    }

    def save(tag:CompoundNBT)
    {
        for (i <- flows.indices)
            tag.putDouble("flow"+i, flows(i))

        tag.putDouble("vl", Vloc)
        tag.putDouble("il", Iloc)
        tag.putDouble("vf", Vflow)
        tag.putDouble("if", Iflow)
        tag.putInt("tm", time)
    }

    def load(tag:CompoundNBT)
    {
        for (i <- flows.indices)
            flows(i) = tag.getDouble("flow"+i)

        Vloc = tag.getDouble("vl")
        Iloc = tag.getDouble("il")
        Vflow = tag.getDouble("vf")
        Iflow = tag.getDouble("if")
        time = tag.getInt("tm")
    }
}

trait TPowerDrawPoint extends PowerConductor
{
    var charge = 0
    var flow = 0

    override def capacitance = 0.25D

    def getChargeScaled(scale:Int) = math.min(scale, scale*charge/1000)
    def getFlowScaled(scale:Int) = Integer.bitCount(flow)*scale/32

    def canWork = charge > 600

    abstract override def update()
    {
        super.update()
        charge = (voltage()*10.0D).asInstanceOf[Int]
        flow <<= 1
        if (canWork) flow |= 1
    }

    abstract override def save(tag:CompoundNBT)
    {
        super.save(tag)
        tag.putInt("chg", charge)
        tag.putInt("flow", flow)
    }

    abstract override def load(tag:CompoundNBT)
    {
        super.load(tag)
        charge = tag.getInt("chg")
        flow = tag.getInt("flow")
    }
}

class JDrawPointPowerConductor(parent:IPowerConnectable, ids:util.Collection[Integer]) extends PowerConductor(parent, ids.asScala.map(Int.unbox).toSeq) with TPowerDrawPoint
{
}

/**
 * Interfaces used by low-load power wires and machines
 */
trait ILowLoadPowerLine
trait ILowLoadMachine
