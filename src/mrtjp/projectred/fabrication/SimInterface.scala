package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.core.vec.Point
import mrtjp.projectred.fabrication.SEIntegratedCircuit._
import net.minecraft.nbt.NBTTagCompound

import scala.collection.mutable.{ListBuffer, Map => MMap}

trait IICSimEngineContainerDelegate
{
    def registersDidChange(registers:Set[Int])

    def ioRegistersDidChange()

    def logDidChange()
}

class ICSimEngineContainer extends ISEICDelegate
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

    var delegate:IICSimEngineContainerDelegate = null

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

    def repropagate()
    {
        simEngine.propagate(this)
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

    override def registersDidChange(registers:Set[Int])
    {
        if (delegate != null)
            delegate.registersDidChange(registers)

        val firstIOReg = REG_IN(0, 0)
        val lastIOReg = REG_OUT(3, 15)
        if (registers.exists {reg => reg >= firstIOReg && reg <= lastIOReg}) { //TODO potentially faster to pull and check
            pullOutputRegisters(0xF)
            if (delegate != null)
                delegate.ioRegistersDidChange()
        }
    }

    override def icDidThrowErrorFlag(flag:Int, registers:Seq[Int], gates:Seq[Int])
    {
        logger.logRuntimeFlag(flag, registers, gates)
        if (delegate != null) delegate.logDidChange()
    }

    def recompileSimulation(map:ISETileMap)
    {
        logger.clear()

        //TODO temporary io check, non-issue once side io modes are stored map-level instead of tile-level
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

        simEngine = ISELinker.linkFromMap(map, logger)
        systemTime = 0
        pushInputRegisters(0xF)
        pushSystemTime()
        pullOutputRegisters(0xF)

        if (delegate != null) delegate.logDidChange()
    }

    def resetSimState(map:ISETileMap)
    {
        for (i <- 0 until 4) iostate(i) = 0
        systemTime = 0
        recompileSimulation(map)
    }

    def saveSimState(tag:NBTTagCompound)
    {
        tag.setBoolean("null_sim", simEngine == null)
        if (simEngine == null) return

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
        if (tag.getBoolean("null_sim")) return

        val io = tag.getIntArray("io_state")
        if (io.length == 4) for (i <- 0 until 4) iostate(i) = io(i)
        systemTime = tag.getLong("sys_time")

        val registers = simEngine.getRegisterMap
        for (i <- 0 until registers.length) {
            val reg = registers(i)
            reg match {
                case StandardRegister(r:Long) => reg.queueVal[Long](tag.getLong(s"reg[$i]"))
                case StandardRegister(r:Int)  => reg.queueVal[Int](tag.getInteger(s"reg[$i]"))
                case StandardRegister(r:Byte) => reg.queueVal[Byte](tag.getByte(s"reg[$i]"))
                case _ => //Dont load the register
            }
            reg.pushVal(simEngine)
        }
    }
}

class SEStatLogger extends ISEStatLogger
{
    private val log = ListBuffer[String]()

    private val warnings = ListBuffer[(Seq[Point], String)]()
    private val errors = ListBuffer[(Seq[Point], String)]()

    //Flags, RegPoints, GatePoints
    private val runtimeFlags = ListBuffer[(Int, Seq[Point], Seq[Point])]()

    private val regIDToPoints = MMap[Int, Set[Point]]()
    private val gateIDToPoints = MMap[Int, Set[Point]]()

    override def clear()
    {
        log.clear()
        warnings.clear()
        errors.clear()
        runtimeFlags.clear()
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

    override def logRuntimeFlag(flag:Int, registers:Seq[Int], gates:Seq[Int])
    {
        runtimeFlags += ((flag, registers.flatMap {regIDToPoints.getOrElse(_, Set.empty)},
                gates.flatMap {gateIDToPoints.getOrElse(_, Set.empty)}))
    }

    override def logRegAlloc(id:Int, points:Set[Point])
    {
        regIDToPoints += id -> points
    }

    override def logGateAlloc(id:Int, points:Set[Point])
    {
        gateIDToPoints += id -> points
    }

    def getWarnings:Seq[(Seq[Point], String)] = warnings
    def getWarningsForPoint(p:Point):Seq[(Seq[Point], String)] = warnings.filter(_._1 contains p)

    def getErrors:Seq[(Seq[Point], String)] = errors
    def getErrorsForPoint(p:Point):Seq[(Seq[Point], String)] = errors.filter(_._1 contains p)

    def getRuntimeFlags:Seq[(Seq[Point], String)] = runtimeFlags.map {p => (p._2 ++ p._3, runtimeFlagToMessage(p._1))}
    def getRuntimeFlagsForPoint(p:Point):Seq[(Seq[Point], String)] = getRuntimeFlags.filter(_._1 contains p)

    private def runtimeFlagToMessage(flag:Int):String = flag match {
        case COMPUTE_OVERFLOW => "COMPUTE OVERFLOW!"
    }

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

        out.writeShort(runtimeFlags.size)
        for ((i, rPoints, gPoints) <- runtimeFlags) {
            out.writeByte(i)

            out.writeByte(rPoints.size)
            for (rp <- rPoints)
                out.writeByte(rp.x).writeByte(rp.y)

            out.writeByte(gPoints.size)
            for (gp <- gPoints)
                out.writeByte(gp.x).writeByte(gp.y)
        }
    }

    def readLog(in:MCDataInput)
    {
        def readList(list:ListBuffer[(Seq[Point], String)])
        {
            list.clear()
            for (_ <- 0 until in.readUShort()) {
                val points = Seq.newBuilder[Point]
                for (_ <- 0 until in.readUShort())
                    points += Point(in.readUByte(), in.readUByte())

                list += points.result() -> in.readString()
            }
        }

        readList(warnings)
        readList(errors)

        runtimeFlags.clear()
        for (_ <- 0 until in.readUShort()) {
            val flag = in.readUByte()
            val rPoints = (0 until in.readUByte()) map {_ => Point(in.readUByte(), in.readUByte())}
            val gPoints = (0 until in.readUByte()) map {_ => Point(in.readUByte(), in.readUByte())}

            runtimeFlags += ((flag, rPoints, gPoints))
        }
    }
}