package mrtjp.projectred.fabrication

import net.minecraft.nbt.NBTTagCompound

class ICSimEngineContainer
{
    import SEIntegratedCircuit._

    var simEngine:SEIntegratedCircuit = null

    var systemTime = 0L

    /**
      * Mapped inputs and outputs of this IC.
      * Outputs go to the world, inputs come in from the world.
      * OOOO OOOO OOOO OOOO IIII IIII IIII IIII
      */
    val iostate = Array(0, 0, 0, 0)

    var ioChangedDelegate = {() => ()}

    var registersChangedDelegate = {_:Set[Int] => ()}

    var propagateSilently = false

    def setInput(r:Int, state:Int)
    {
        iostate(r) = iostate(r)&0xFFFF0000|state&0xFFFF
    }

    def onInputChanged(mask:Int)
    {
        pushInputRegisters(mask)
    }

    def advanceTime(ticks:Long)
    {
        systemTime += ticks
        pushSystemTime()
    }

    def setOutput(r:Int, state:Int)
    {
        iostate(r) = iostate(r)&0xFFFF|(state&0xFFFF)<<16
    }

    def propagateAll()
    {
        simEngine.propagateInitial()
    }

    def repropagate()
    {
        simEngine.repropagate()
    }

    private def pushInputRegisters(mask:Int)
    {
        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            val input = iostate(r)&0xFFFF
            for (i <- 0 until 16)
                simEngine.queueRegVal[Byte](REG_IN(r, i), if ((input&1<<i) != 0) 1 else 0)
        }
    }

    private def pushSystemTime()
    {
        simEngine.queueRegVal(REG_SYSTIME, systemTime)
    }

    private def pullOutputRegisters(mask:Int) //TODO perhaps remove mask and just pull everything??
    {
        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            var output = 0
            for (i <- 0 until 16) if (simEngine.getRegVal(REG_OUT(r, i)) != 0)
                output |= 1<<i
            setOutput(r, output)
        }
    }

    private def onRegistersChanged(changes:Set[Int])
    {
        if (propagateSilently) return
        registersChangedDelegate(changes)
        val firstIOReg = REG_IN(0, 0)
        val lastIOReg = REG_OUT(3, 15)
        if (changes.exists {reg => reg >= firstIOReg && reg <= lastIOReg}) {
            pullOutputRegisters(0xFF)
            ioChangedDelegate()
        }
    }

    def recompileSimulation(map:ISETileMap)
    {
        simEngine = ISELinker.linkFromMap(map, onRegistersChanged)
        pushInputRegisters(0xF)
        pushSystemTime()
    }

    def resetSimState(map:ISETileMap)
    {
        for (i <- 0 until 4) iostate(i) = 0
        systemTime = 0
        recompileSimulation(map)
    }

    def saveSimState(tag:NBTTagCompound)
    {
        tag.setIntArray("io_state", iostate)
        tag.setLong("sys_time", systemTime)

        val registers = simEngine.getRegisterMap
        for (i <- 0 until registers.length) {
            registers(i) match {
                case StandardRegister(r:Long) => tag.setLong(s"reg[$i]", r)
                case StandardRegister(r:Int)  => tag.setInteger(s"reg[$i]", r)
                case StandardRegister(r:Byte) => tag.setByte(s"reg[$i]", r)
                case _ => //Dont save the register
            }
        }
    }

    def loadSimState(tag:NBTTagCompound)
    {
        val io = tag.getIntArray("io_state")
        if (io.length == 4) for (i <- 0 until 4) iostate(i) = io(i)
        systemTime = tag.getLong("sys_time")

        val registers = simEngine.getRegisterMap
        for (i <- 0 until registers.length) {
            registers(i) match {
                case StandardRegister(r:Long) => simEngine.queueRegVal[Long](i, tag.getLong(s"reg[$i]"))
                case StandardRegister(r:Int)  => simEngine.queueRegVal[Int](i, tag.getInteger(s"reg[$i]"))
                case StandardRegister(r:Byte) => simEngine.queueRegVal[Byte](i, tag.getByte(s"reg[$i]"))
                case _ => //Dont load the register
            }
        }
    }
}

case class StandardRegister[Type](var value:Type) extends ISERegister
{
    var queuedVal:Type = value

    override def getVal[T] = value.asInstanceOf[T]

    override def queueVal[T](newVal:T) =
    {
        queuedVal = newVal.asInstanceOf[Type]
        value != queuedVal
    }

    override def pushVal(ic:SEIntegratedCircuit)
    {
        value = queuedVal
    }
}

class ConstantRegister[Type](c:Type) extends ISERegister
{
    override def getVal[T] = c.asInstanceOf[T]
    override def queueVal[T](newVal:T) = false
    override def pushVal(ic:SEIntegratedCircuit){}
}