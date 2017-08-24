package mrtjp.projectred.fabrication

import java.util.Random

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import com.mojang.realmsclient.gui.ChatFormatting._
import mrtjp.projectred.ProjectRedCore.log
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.fabrication.SEIntegratedCircuit._
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.mutable.ListBuffer

trait TComplexGateICTile extends GateICTile
{
    def getLogicComplex = getLogic[TComplexGateTileLogic[TComplexGateICTile]]

    def assertLogic()

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        getLogicComplex.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        assertLogic()
        getLogicComplex.load(tag)
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        getLogicComplex.writeDesc(packet)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        assertLogic()
        getLogicComplex.readDesc(packet)
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case k if k > 10 =>
            assertLogic() //this may be a net dump part
            getLogicComplex.read(packet, k)
        case _ => super.read(packet, key)
    }

    abstract override def preparePlacement(rot:Int, meta:Int)
    {
        super.preparePlacement(rot, meta)
        assertLogic()
    }
}

class SequentialGateICTile extends RedstoneGateICTile with TComplexGateICTile
{
    var logic:SequentialICGateLogic = null

    override def assertLogic()
    {
        if (logic == null) logic = SequentialICGateLogic.create(this, subID)
    }

    override def getLogic[T]:T = logic.asInstanceOf[T]

    override def getPartType = ICTileDefs.ComplexGate

    override def readClientPacket(in:MCDataInput, key:Int) = key match
    {
        case 3 => getLogicPrimitive match {
            case t:ITimerGuiLogic => t.setTimerMax(this, t.getTimerMax+in.readShort())
            case _ => log.error("Server IC stream received client packet for incorrect gate type")
        }
//        case 4 => getLogicPrimitive match {
//            case t:ICounterGuiLogic =>
//                val actionID = in.readByte()
//                actionID match {
//                    case 0 => t.setCounterMax(this, t.getCounterMax+in.readShort())
//                    case 1 => t.setCounterIncr(this, t.getCounterIncr+in.readShort())
//                    case 2 => t.setCounterDecr(this, t.getCounterDecr+in.readShort())
//                    case _ => log.error("Server IC stream received client packet for incorrect gate type")
//                }
//            case _ => log.error("Server IC stream received client packet for incorrect gate type")
//        }
        case _ => super.readClientPacket(in, key)
    }
}

trait TComplexGateTileLogic[T <: TComplexGateICTile] extends GateTileLogic[T]
{
    def save(tag:NBTTagCompound){}
    def load(tag:NBTTagCompound){}

    def readDesc(packet:MCDataInput){}
    def writeDesc(packet:MCDataOutput){}

    /*
     * Allocated keys > 10
     */
    def read(packet:MCDataInput, key:Int){}
}

object SequentialICGateLogic
{
    import mrtjp.projectred.fabrication.{ICGateDefinition => defs}

    def create(gate:SequentialGateICTile, subID:Int):SequentialICGateLogic = subID match
    {
        case defs.Pulse.ordinal => new Pulse(gate)
        case defs.Repeater.ordinal => new Repeater(gate)
        case defs.Randomizer.ordinal => new Randomizer(gate)
        case defs.SRLatch.ordinal => new SRLatch(gate)
        case defs.ToggleLatch.ordinal => new ToggleLatch(gate)
        case defs.TransparentLatch.ordinal => new TransparentLatch(gate)
        case defs.Timer.ordinal => new Timer(gate)
//        case defs.Sequencer.ordinal => new Sequencer(gate)
//        case defs.Counter.ordinal => new Counter(gate)
//        case defs.StateCell.ordinal => new StateCell(gate)
//        case defs.Synchronizer.ordinal => new Synchronizer(gate)
        case _ => throw new IllegalArgumentException("Invalid gate subID: "+subID)
    }
}

abstract class SequentialICGateLogic(val gate:SequentialGateICTile) extends RedstoneGateTileLogic[SequentialGateICTile] with TComplexGateTileLogic[SequentialGateICTile]
{
    val inputRegs = Array(-1, -1, -1, -1)
    val outputRegs = Array(-1, -1, -1, -1)

    def cacheIORegisters(linker:ISELinker)
    {
        for (r <- 0 until 4) {
            inputRegs(r) =
                    if (canInput(gate, r)) gate.getInputRegister(r, linker) else -1
            outputRegs(r) =
                    if (canOutput(gate, r)) gate.getOutputRegister(r, linker) else -1
        }

        import SEIntegratedCircuit._
        if (inputRegs.forall(id => id == -1 || id == REG_ZERO))
            linker.getLogger.logWarning(Seq(gate.pos), "gate has no inputs")
        if (outputRegs.forall(id => id == -1 || id == REG_ZERO))
            linker.getLogger.logWarning(Seq(gate.pos), "gate has no outputs")
    }

    private def pullInput(mask:Int) = //Pull the input from the sim engine
    {
        var input = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            if (gate.editor.simEngineContainer.simEngine.getRegVal[Byte](inputRegs(r)) > 0) input |= 1<<r
        }
        input
    }

    private def pullOutput(mask:Int) = //Pull the output form the sim engine
    {
        var output = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            if (gate.editor.simEngineContainer.simEngine.getRegVal[Byte](outputRegs(r)) > 0) output |= 1<<r
        }
        output
    }

    def pullIOStateFromSim()
    {
        val oldState = gate.state
        val newState = pullInput(inputMask(gate.shape))&0xF | pullOutput(outputMask(gate.shape))<<4
        if (oldState != newState) {
            gate.setState(newState)
            gate.sendStateUpdate()
        }
    }

    override def allocateOrFindRegisters(gate:SequentialGateICTile, linker:ISELinker)
    {
        cacheIORegisters(linker)
        allocInternalRegisters(linker)
    }

    override def onRegistersChanged(gate:SequentialGateICTile, regIDs:Set[Int])
    {
        pullIOStateFromSim()
    }

    def allocInternalRegisters(linker:ISELinker){}
}

class Pulse(gate:SequentialGateICTile) extends SequentialICGateLogic(gate)
{
    var stateReg = -1
    var schdTimeReg = -1

    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def allocInternalRegisters(linker:ISELinker)
    {
        stateReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(stateReg, new StandardRegister[Byte](0))

        schdTimeReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(schdTimeReg, new StandardRegister[Long](-1))
    }

    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker) =
    {
        val outputReg = outputRegs(0)
        val inputReg = inputRegs(2)
        val stateReg = this.stateReg
        val schdTimeReg = this.schdTimeReg

        val calculation = new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                ic.getRegVal[Byte](stateReg) match {
                    case 0 => //Wait for high input state
                        if (ic.getRegVal[Byte](inputReg) != 0) {
                            ic.queueRegVal[Byte](stateReg, 1)
                            ic.queueRegVal[Byte](outputReg, 1)
                            ic.queueRegVal[Long](schdTimeReg, ic.getRegVal[Long](REG_SYSTIME)+2)
                        }
                    case 1 => //Wait for timer expire state
                        if (ic.getRegVal[Long](REG_SYSTIME) >= ic.getRegVal[Long](schdTimeReg)) {
                            ic.queueRegVal[Byte](stateReg, if (ic.getRegVal[Byte](inputReg) == 0) 0 else 2)
                            ic.queueRegVal[Byte](outputReg, 0)
                            ic.queueRegVal[Long](schdTimeReg, -1)
                        }
                    case 2 => //Wait for low input state
                        if (ic.getRegVal[Byte](inputReg) == 0)
                            ic.queueRegVal[Byte](stateReg, 0)
                }
            }
        }

        val gateID = linker.allocateGateID(Set(gate.pos))
        linker.addGate(gateID, calculation, Seq(inputReg, REG_SYSTIME), Seq(outputReg, stateReg, schdTimeReg))
    }
}

class Repeater(gate:SequentialGateICTile) extends SequentialICGateLogic(gate)
{
    val delays = Array(2, 4, 6, 8, 16, 32, 64, 128, 256)

    var stateReg = -1
    var schdTimeReg = -1

    override def outputMask(shape:Int) = 1
    override def inputMask(shape:Int) = 4

    override def cycleShape(gate:SequentialGateICTile) =
    {
        gate.setShape((gate.shape+1)%delays.length)
        true
    }

    override def activate(gate:SequentialGateICTile)
    {
        gate.configure()
    }

    @SideOnly(Side.CLIENT)
    override def buildRolloverData(gate:SequentialGateICTile, buffer:ListBuffer[String])
    {
        super.buildRolloverData(gate, buffer)
        buffer += GRAY+"delay: "+delays(gate.shape)
    }

    override def allocInternalRegisters(linker:ISELinker)
    {
        stateReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(stateReg, new StandardRegister[Byte](0))

        schdTimeReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(schdTimeReg, new StandardRegister[Long](-1))
    }

    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
    {
        val outputReg = outputRegs(0)
        val inputReg = inputRegs(2)
        val stateReg = this.stateReg
        val schdTimeReg = this.schdTimeReg
        val delay = delays(gate.shape)

        val calculation = new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {

                def inputHi = ic.getRegVal[Byte](inputReg) != 0
                def sysTime = ic.getRegVal[Long](REG_SYSTIME)
                def schdTime = ic.getRegVal[Long](schdTimeReg)

                def enterWaitForHiState() {
                    ic.queueRegVal[Byte](stateReg, 0)
                    ic.queueRegVal[Byte](outputReg, 0)
                    ic.queueRegVal[Long](schdTimeReg, -1)
                }

                def enterOutputLoDelayState() {
                    ic.queueRegVal[Byte](stateReg, 1)
                    ic.queueRegVal[Byte](outputReg, 0)
                    ic.queueRegVal[Long](schdTimeReg, sysTime+delay)
                }

                def enterWaitForLoState() {
                    ic.queueRegVal[Byte](stateReg, 2)
                    ic.queueRegVal[Byte](outputReg, 1)
                    ic.queueRegVal[Long](schdTimeReg, -1)
                }

                def enterOutputHiDelayState() {
                    ic.queueRegVal[Byte](stateReg, 3)
                    ic.queueRegVal[Byte](outputReg, 1)
                    ic.queueRegVal[Long](schdTimeReg, sysTime+delay)
                }

                ic.getRegVal[Byte](stateReg) match {
                    case 0 => //Wait for high input state
                        if (inputHi)
                            enterOutputLoDelayState()
                    case 1 => //Output delay lo state
                        if (sysTime >= schdTime)
                            enterWaitForLoState()
                    case 2 => //Wait for low state
                        if (!inputHi)
                            enterOutputHiDelayState()
                    case 3 => //Output delay hi state
                        if (sysTime >= schdTime)
                            enterWaitForHiState()
                }
            }
        }

        val gateID = linker.allocateGateID(Set(gate.pos))
        linker.addGate(gateID, calculation, Seq(inputReg, REG_SYSTIME), Seq(outputReg, stateReg, schdTimeReg))
    }
}

class Randomizer(gate:SequentialGateICTile) extends SequentialICGateLogic(gate) with TIOControlableGateTileLogic[SequentialGateICTile]
{
    var stateReg = -1
    var timeStartReg = -1

    override def outputMask(shape:Int) = ~((shape&1)<<1|(shape&2)>>1|(shape&4)<<1)&0xB
    override def inputMask(shape:Int) = 4

    override def deadSides = 3

    override def allocInternalRegisters(linker:ISELinker)
    {
        stateReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(stateReg, new StandardRegister[Byte](127))

        timeStartReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(timeStartReg, new StandardRegister[Long](-1))
    }

    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
    {
        val outputAReg = if (outputRegs(3) != -1) outputRegs(3) else REG_ZERO
        val outputBReg = if (outputRegs(0) != -1) outputRegs(0) else REG_ZERO
        val outputCReg = if (outputRegs(1) != -1) outputRegs(1) else REG_ZERO
        val inputReg = inputRegs(2)

        val stateReg = this.stateReg
        val timeStartReg = this.timeStartReg

        val calculation = new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {

                def inputHi = ic.getRegVal[Byte](inputReg) != 0

                def sysTime = ic.getRegVal[Long](REG_SYSTIME)
                def startTime = ic.getRegVal[Long](timeStartReg)

                def enterShiftingState() {
                    ic.queueRegVal[Byte](stateReg, 0)
                    ic.queueRegVal[Long](timeStartReg, ic.getRegVal[Long](REG_SYSTIME))
                }

                def enterHaltState() {
                    ic.queueRegVal[Byte](stateReg, 1)
                    ic.queueRegVal[Long](timeStartReg, -1)
                }

                def randomizeOutput() {
                    val sMask = Randomizer.rand.nextInt(8)
                    ic.queueRegVal[Byte](outputAReg, if ((sMask&1) != 0) 1 else 0)
                    ic.queueRegVal[Byte](outputBReg, if ((sMask&2) != 0) 1 else 0)
                    ic.queueRegVal[Byte](outputCReg, if ((sMask&4) != 0) 1 else 0)
                }

                ic.getRegVal[Byte](stateReg) match {
                    case 0 => //Shifting state
                        if (!inputHi)
                            enterHaltState()
                        else if ((sysTime-startTime)%2 == 0)
                            randomizeOutput()
                    case 1 => //Halt state
                        if (inputHi)
                            enterShiftingState()
                    case 127 => //Initial state
                        if (inputHi)
                            enterShiftingState()
                        else
                            enterHaltState()
                }
            }
        }

        val gateID = linker.allocateGateID(Set(gate.pos))
        linker.addGate(gateID, calculation, Seq(inputReg, REG_SYSTIME),
            Seq(outputAReg, outputBReg, outputCReg, stateReg, timeStartReg).filter(_ != REG_ZERO))
    }
}

object Randomizer {
    val rand = new Random
}

class SRLatch(gate:SequentialGateICTile) extends SequentialICGateLogic(gate)
{
    var stateReg = -1

    override def outputMask(shape:Int) = if ((shape>>1) == 0) 0xF else 5
    override def inputMask(shape:Int) = 0xA

    override def cycleShape(gate:SequentialGateICTile) =
    {
        gate.setShape((gate.shape+1)%4)
        true
    }

    def reflect = (gate.shape&1) != 0
    def backfeed = (gate.shape&2) == 0

    override def allocInternalRegisters(linker:ISELinker)
    {
        stateReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(stateReg, new StandardRegister[Byte](127))
    }

    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
    {
        val outputAReg = outputRegs(2)
        val outputBReg = outputRegs(0)
        val bfOutputAReg = if (backfeed) if (reflect) outputRegs(3) else outputRegs(1) else REG_ZERO
        val bfOutputBReg = if (backfeed) if (reflect) outputRegs(1) else outputRegs(3) else REG_ZERO

        val inputAReg = if (reflect) inputRegs(3) else inputRegs(1)
        val inputBReg = if (reflect) inputRegs(1) else inputRegs(3)

        val stateReg = this.stateReg

        val calculation = new ISEGate {
            val rand = new Random()

            override def compute(ic:SEIntegratedCircuit) {

                def enterAState() {
                    ic.queueRegVal[Byte](stateReg,     0)
                    ic.queueRegVal[Byte](outputAReg,   1)
                    ic.queueRegVal[Byte](bfOutputAReg, 1)
                    ic.queueRegVal[Byte](outputBReg,   0)
                    ic.queueRegVal[Byte](bfOutputBReg, 0)
                }

                def enterBState() {
                    ic.queueRegVal[Byte](stateReg,     1)
                    ic.queueRegVal[Byte](outputAReg,   0)
                    ic.queueRegVal[Byte](bfOutputAReg, 0)
                    ic.queueRegVal[Byte](outputBReg,   1)
                    ic.queueRegVal[Byte](bfOutputBReg, 1)
                }

                def enterUndfState() {
                    ic.queueRegVal[Byte](stateReg,     2)
                    ic.queueRegVal[Byte](outputAReg,   0)
                    ic.queueRegVal[Byte](bfOutputAReg, 0)
                    ic.queueRegVal[Byte](outputBReg,   0)
                    ic.queueRegVal[Byte](bfOutputBReg, 0)
                }

                def inputMask = (if (ic.getRegVal[Byte](inputBReg) != 0) 2 else 0) | (if (ic.getRegVal[Byte](inputAReg) != 0) 1 else 0)

                ic.getRegVal[Byte](stateReg) match {
                    case 0 => //A State
                        inputMask match {
                            case 2 => enterBState()    //A-lo B-hi
                            case 3 => enterUndfState() //A-hi B-hi
                            case 0 | 1 => //Remain in state
                        }
                    case 1 => //B State
                        inputMask match {
                            case 1 => enterAState() //A-hi B-lo
                            case 3 => enterUndfState() //A-hi B-hi
                            case 0 | 2 => //Remain in state
                        }
                    case 2 => //Undefined State
                        inputMask match {
                            case 0 => if (rand.nextBoolean()) enterAState() else enterBState()
                            case 1 => enterAState()
                            case 2 => enterBState()
                            case 3 => //Still Undf, Remain in state
                        }
                    case 127 => //initial state
                        inputMask match {
                            case 2 => enterBState()
                            case 3 => enterUndfState()
                            case 0 | 1 => enterAState()
                        }
                }
            }
        }

        val gateID = linker.allocateGateID(Set(gate.pos))
        linker.addGate(gateID, calculation, Seq(inputAReg, inputBReg),
            Seq(outputAReg, outputBReg, bfOutputAReg, bfOutputBReg, stateReg).filter(_ != REG_ZERO))
    }
}

class ToggleLatch(gate:SequentialGateICTile) extends SequentialICGateLogic(gate)
{
    var stateReg = -1
    var prevInputMaskReg = -1

    override def outputMask(shape:Int) = 5
    override def inputMask(shape:Int) = 0xA

    override def cycleShape(gate:SequentialGateICTile) =
    {
        gate.setShape(gate.shape^1)
        true
    }

    override def activate(gate:SequentialGateICTile)
    {
        gate.configure()
    }

    override def allocInternalRegisters(linker:ISELinker)
    {
        stateReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(stateReg, new StandardRegister[Byte](127))

        prevInputMaskReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(prevInputMaskReg, new StandardRegister[Byte](0))

    }

    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
    {
        val outputAReg = outputRegs(0)
        val outputBReg = outputRegs(2)
        val inputAReg = inputRegs(3)
        val inputBReg = inputRegs(1)
        val stateReg = this.stateReg
        val prevInputMaskReg = this.prevInputMaskReg
        val defState = gate.shape

        val calculation = new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                def enterAState() {
                    ic.queueRegVal[Byte](stateReg, 0)
                    ic.queueRegVal[Byte](outputAReg, 1)
                    ic.queueRegVal[Byte](outputBReg, 0)
                }

                def enterBState() {
                    ic.queueRegVal[Byte](stateReg, 1)
                    ic.queueRegVal[Byte](outputAReg, 0)
                    ic.queueRegVal[Byte](outputBReg, 1)
                }

                val inputMask = (if (ic.getRegVal[Byte](inputBReg) != 0) 2 else 0) | (if (ic.getRegVal[Byte](inputAReg) != 0) 1 else 0)

                def singleBitHi = {
                    val high = inputMask & ~ic.getRegVal[Byte](prevInputMaskReg)
                    high == 1 || high == 2
                }

                ic.getRegVal[Byte](stateReg) match {
                    case 0 => if (singleBitHi) enterBState()
                    case 1 => if (singleBitHi) enterAState()
                    case 127 => if (defState == 0) enterAState() else enterBState()
                }

                ic.queueRegVal[Byte](prevInputMaskReg, inputMask.toByte)
            }
        }

        val gateID = linker.allocateGateID(Set(gate.pos))
        linker.addGate(gateID, calculation, Seq(inputAReg, inputBReg), Seq(outputAReg, outputBReg, stateReg, prevInputMaskReg))
    }
}

trait ITimerGuiLogic
{
    def getTimerMax:Int
    def setTimerMax(gate:GateICTile, t:Int)
}

trait TTimerICGateLogic extends SequentialICGateLogic with ITimerGuiLogic
{
    var pointer_max = 38
    var pointer_start = -1L

    var timerStartReg = -1

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setInteger("pmax", pointer_max)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        pointer_max = tag.getInteger("pmax")
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeInt(pointer_max)
        packet.writeLong(pointer_start)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        pointer_max = packet.readInt()
        pointer_start = packet.readLong()
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 12 => pointer_max = packet.readInt()
        case 13 =>
            pointer_start = packet.readInt()
            if (pointer_start >= 0) pointer_start = getTotalTime-pointer_start
        case _ => super.read(packet, key)
    }

    def getTotalTime = gate.editor.network.getEditorWorld.getTotalWorldTime

    def pointerValue = if (pointer_start < 0) 0 else (getTotalTime-pointer_start).toInt

    def sendPointerMaxUpdate(){ gate.writeStreamOf(12).writeInt(pointer_max) }
    def sendPointerUpdate(){ gate.writeStreamOf(13).writeInt(if (pointer_start < 0) -1 else pointerValue)}

    override def getTimerMax = pointer_max+2
    override def setTimerMax(gate:GateICTile, time:Int)
    {
        var t = time
        val minTime = math.max(4, Configurator.minTimerTicks)
        if (t < minTime) t = minTime
        if (t != getTimerMax) {
            pointer_max = t-2
            sendPointerMaxUpdate()
            gate.editor.network.markSave()
            gate.onSchematicChanged()
        }
    }

    override def onRegistersChanged(gate:SequentialGateICTile, regIDs:Set[Int])
    {
        super.onRegistersChanged(gate, regIDs)

        //Update pointer_start
        val old_pointer_start = pointer_start
        pointer_start = gate.editor.simEngineContainer.simEngine.getRegVal[Long](timerStartReg)
        if (pointer_start > 0) //convert time domains
            pointer_start += getTotalTime-gate.editor.simEngineContainer.simEngine.getRegVal[Long](REG_SYSTIME)
        if (old_pointer_start != pointer_start)
            sendPointerUpdate()
    }

    def interpPointer(f:Float) = if (pointer_start < 0) 0f else (pointerValue+f)/pointer_max

    @SideOnly(Side.CLIENT)
    override def createGui(gate:SequentialGateICTile):ICTileGui = new ICTimerGateGui(gate)

    @SideOnly(Side.CLIENT)
    override def buildRolloverData(gate:SequentialGateICTile, buffer:ListBuffer[String]) =
    {
        super.buildRolloverData(gate, buffer)
        buffer += GRAY+"interval: "+"%.2f".format(getTimerMax*0.05)+"s"
    }
}

class TransparentLatch(gate:SequentialGateICTile) extends SequentialICGateLogic(gate)
{
    var stateReg = -1

    override def outputMask(shape:Int) = if (shape == 0) 3 else 9
    override def inputMask(shape:Int) = if (shape == 0) 0xC else 6

    override def cycleShape(gate:SequentialGateICTile) =
    {
        gate.setShape(gate.shape^1)
        true
    }

    override def allocInternalRegisters(linker:ISELinker)
    {
        stateReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(stateReg, new StandardRegister[Byte](127))
    }

    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
    {
        val output1Reg = outputRegs(0)
        val output2Reg = if (gate.shape == 0) outputRegs(1) else outputRegs(3)
        val dataInReg = if (gate.shape == 0) inputRegs(3) else inputRegs(1)
        val wrEnableReg = inputRegs(2)
        val stateReg = this.stateReg

        val calculation = new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {

                def dataWrHi = ic.getRegVal[Byte](wrEnableReg) != 0

                def enterLockState() {
                    ic.queueRegVal[Byte](stateReg, 0)
                }

                def enterWriteState() {
                    ic.queueRegVal[Byte](stateReg, 1)
                    writeData()
                }

                def writeData() {
                    val data = ic.getRegVal[Byte](dataInReg)
                    ic.queueRegVal[Byte](output1Reg, data)
                    ic.queueRegVal[Byte](output2Reg, data)
                }

                ic.getRegVal[Byte](stateReg) match {
                    case 0 => //lock state
                        if (dataWrHi)
                            enterWriteState()
                    case 1 => //wr state
                        if (dataWrHi)
                            writeData()
                        else
                            enterLockState()
                    case 127 => //initial state
                        if (dataWrHi)
                            enterWriteState()
                        else
                            enterLockState()
                }
            }
        }

        val gateID = linker.allocateGateID(Set(gate.pos))
        linker.addGate(gateID, calculation, Seq(dataInReg, wrEnableReg), Seq(output1Reg, output2Reg, stateReg))
    }
}

class Timer(gate:SequentialGateICTile) extends SequentialICGateLogic(gate) with TTimerICGateLogic
{
    var stateReg = -1

    override def outputMask(shape:Int) = 0xB
    override def inputMask(shape:Int) = 0xE

    override def allocInternalRegisters(linker:ISELinker)
    {
        stateReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(stateReg, new StandardRegister[Byte](2))

        timerStartReg = linker.allocateRegisterID(Set(gate.pos))
        linker.addRegister(timerStartReg, new StandardRegister[Long](-1))
    }

    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
    {
        val output1Reg = outputRegs(3)
        val output2Reg = outputRegs(0)
        val output3Reg = outputRegs(1)

        val input1Reg = inputRegs(1)
        val input2Reg = inputRegs(2)
        val input3Reg = inputRegs(3)

        val stateReg = this.stateReg
        val timerStartReg = this.timerStartReg
        val timerMax = getTimerMax

        val calculation = new ISEGate {
            override def compute(ic:SEIntegratedCircuit) {
                val sysTime = ic.getRegVal[Long](REG_SYSTIME)

                def inputHi = ic.getRegVal[Byte](input1Reg) != 0 ||
                        ic.getRegVal[Byte](input2Reg) != 0 ||
                        ic.getRegVal[Byte](input3Reg) != 0

                def setOutputs(v:Byte) {
                    ic.queueRegVal[Byte](output1Reg, v)
                    ic.queueRegVal[Byte](output2Reg, v)
                    ic.queueRegVal[Byte](output3Reg, v)
                }

                def enterCountState() {
                    ic.queueRegVal[Byte](stateReg, 0)
                    ic.queueRegVal[Long](timerStartReg, sysTime)
                    setOutputs(0)
                }

                def enterTickState() {
                    ic.queueRegVal[Byte](stateReg, 1)
                    setOutputs(1)
                }

                def enterHaltState() {
                    ic.queueRegVal[Byte](stateReg, 2)
                    ic.queueRegVal[Long](timerStartReg, -1)
                    setOutputs(0)
                }

                ic.getRegVal[Byte](stateReg) match {
                    case 0 => //Counting state
                        if (inputHi)
                            enterHaltState()
                        else if (sysTime >= ic.getRegVal[Long](timerStartReg)+timerMax-2)
                            enterTickState()
                    case 1 => //Tick state
                        if (sysTime >= ic.getRegVal[Long](timerStartReg)+timerMax)
                            if (!inputHi) enterCountState() else enterHaltState()
                    case 2 => if (!inputHi) enterCountState() //Halt state
                }
            }
        }

        val gateID = linker.allocateGateID(Set(gate.pos))
        linker.addGate(gateID, calculation, Seq(input1Reg, input2Reg, input3Reg, REG_SYSTIME),
            Seq(output1Reg, output2Reg, output3Reg, stateReg, timerStartReg))
    }
}