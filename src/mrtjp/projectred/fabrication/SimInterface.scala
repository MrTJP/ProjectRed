package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.core.vec.Point
import net.minecraft.nbt.NBTTagCompound

import scala.collection.mutable.ListBuffer

class ICSimEngineContainer
{
    import SEIntegratedCircuit._

    var simEngine:SEIntegratedCircuit = null

    val logger = new SEStatLogger

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
        logger.clear()

        //temporary io check, non-issue once side io modes are stored map-level instead of tile-level
        val ioParts = map.tiles.collect {
            case (pos, io:IIOGateTile) => (pos, io)
        }
        for (s <- 0 until 4) {
            val sio = ioParts.filter(_._2.getIOSide == s)
            if (sio.size > 1) {
                val m = sio.head._2.getIOMode
                val c = sio.head._2.getConnMode
                val p = sio.keys.map{p => Point(p._1, p._2)}.toSeq

                if (sio.exists(_._2.getIOMode != m))
                    logger.logError(p, "io direction conflict")
                if (sio.exists(_._2.getConnMode != c))
                    logger.logError(p, "io connection type conflict")
            }
        }

        simEngine = ISELinker.linkFromMap(map, onRegistersChanged, logger)
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

class SEStatLogger extends ISEStatLogger
{
    val log = ListBuffer[String]()

    val warnings = ListBuffer[(Seq[Point], String)]()
    val errors = ListBuffer[(Seq[Point], String)]()

    override def clear()
    {
        log.clear()
        warnings.clear()
        errors.clear()
    }

    override def logInfo(message:String)
    {
        log += message
    }

    override def logWarning(points:Seq[Point], message:String)
    {
        warnings += points -> message
    }

    override def logError(points:Seq[Point], message:String)
    {
        errors += points -> message
    }

    def getWarningsForPoint(p:Point):Seq[(Seq[Point], String)] = warnings.filter(_._1 contains p)

    def getErrorsForPoint(p:Point):Seq[(Seq[Point], String)] = errors.filter(_._1 contains p)

    def writeLog(out:MCDataOutput)
    {
        def writeList(list:ListBuffer[(Seq[Point], String)]) {
            val s = list.size
            out.writeShort(s)
            for (i <- 0 until s) {
                val (points, desc) = list(i)
                val s2 = points.size
                out.writeShort(s2)
                for (p <- points)
                    out.writeByte(p.x).writeByte(p.y)
                out.writeString(desc)
            }
        }

        writeList(warnings)
        writeList(errors)
    }

    def readLog(in:MCDataInput)
    {
        def readList(list:ListBuffer[(Seq[Point], String)])
        {
            list.clear()
            for (_ <- 0 until in.readUShort()) {
                val points = Seq.newBuilder[Point]
                for (_ <- 0 until in.readUShort())
                    points += new Point(in.readUByte(), in.readUByte())

                list += points.result() -> in.readString()
            }
        }

        readList(warnings)
        readList(errors)
    }
}