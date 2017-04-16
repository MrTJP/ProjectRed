package mrtjp.projectred.fabrication

import scala.collection.mutable
import scala.collection.mutable.{Set => MSet}

class SEIntegratedCircuit(
    $registers:Seq[ISERegister], //Registers in the circuit, indexed by regID
    $gates:Seq[ISEGate], //Gates in the circuit, indexed by gateID
    $regDependents:Map[Int, Seq[Int]], //Register deps [regID -> Seq(gateID)]
    registerChangeDelegate:Set[Int] => Unit
)
{
    val registers = $registers.toArray
    val gates = $gates.toArray

    val regDependents = {
        val b = mutable.ArrayBuilder.make[Array[Int]]
        for (i <- 0 until registers.length)
            b += $regDependents.getOrElse(i, Seq.empty).toArray
        b.result()
    }

    val changeQueue = Seq.newBuilder[Int] //changed registers [regID]

    def getRegisterMap:Seq[ISERegister] = registers

    def getRegVal[T](regID:Int):T = registers(regID).getVal[T]

    def queueRegVal[T](regID:Int, newVal:T)
    {
        if (registers(regID).queueVal[T](newVal))
            changeQueue += regID
    }

    def propagateInitial()
    {
        for (regID <- 0 until registers.size)
            changeQueue += regID //mark all registers dirty
        repropagate()
    }

    def repropagate()
    {
        val allChanges = Set.newBuilder[Int]
        var changes:Seq[Int] = null

        def fetch() {
            changes = changeQueue.result()
            changeQueue.clear()
            allChanges ++= changes
        }
        fetch()

        do {
            for (regID <- changes)
                registers(regID).pushVal(this)

            val computes = MSet[Int]()
            for (regID <- changes) {
                for (gateID <- regDependents(regID)) {
                    if (!computes(gateID)) {
                        gates(gateID).compute(this)
                        computes += gateID
                    }
                }
            }

            fetch()
        }
        while (changes.nonEmpty)

        val ch = allChanges.result()
        if (ch.nonEmpty) registerChangeDelegate(ch)
    }
}

object SEIntegratedCircuit
{
    //Reserved registers
    val REG_IN_BASE = 0
    def REG_IN(r:Int, o:Int) = REG_IN_BASE+r*16+o

    val REG_OUT_BASE = 64
    def REG_OUT(r:Int, o:Int) = REG_OUT_BASE+r*16+o

    val REG_SYSTIME = 128

    val REG_ZERO = 129
}

trait ISERegister
{
    def getVal[T]:T

    def queueVal[T](newVal:T):Boolean

    def pushVal(ic:SEIntegratedCircuit)
}

trait ISEGate
{
    def compute(ic:SEIntegratedCircuit)
}