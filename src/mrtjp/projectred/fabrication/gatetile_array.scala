package mrtjp.projectred.fabrication

trait TArrayGateICTile extends RedstoneGateICTile with IRedwireICPart with IWireICTile with ISEWireTile
{
    def getLogicArray = getLogic[TArrayGateTileLogic[TArrayGateICTile]]

    def getStateRegister(r:Int, linker:ISELinker):Int =  linker.getWirenetOutputRegister(pos, toAbsolute(r))

    override def isNetOutput(r:Int):Boolean =
    {
        if (maskConnects(r)) getStraight(r) match {
            case gate:IRedwireICGate =>
                if(gate.canInputFrom(rotFromStraight(r))) return true
            case _ =>
        }
        false
    }

    override def isNetInput(r:Int):Boolean =
    {
        if (maskConnects(r)) getStraight(r) match {
            case gate:IRedwireICGate =>
                if (gate.canOutputTo(rotFromStraight(r))) return true
            case _ =>
        }
        false
    }

    override def getConnType(r:Int) = getLogicArray.getConnType(r)

    override def getInputColourMask(r:Int) = getLogicArray.getInputColourMask(r)
    override def getOutputColourMask(r:Int) = getLogicArray.getOutputColourMask(r)

    override def getPropMask(r:Int) = getLogicArray.getPropMask(r)

    override def isConnected(r:Int) = maskConnects(r)

    override def buildWireNet(r:Int) = {
        val wireNet = new WireNet(tileMap, pos, getPropMask(r))
        wireNet.calculateNetwork()
        wireNet
    }

    override def cacheStateRegisters(linker:ISELinker){}
}

trait TArrayGateTileLogic[T <: TArrayGateICTile] extends RedstoneGateTileLogic[T]
{
    override def canConnectTo(gate:T, part:ICTile, r:Int) = part match {
        case re:IRedwireICPart if canConnectRedwire(gate, r) => true
        case _ => super.canConnectTo(gate, part, r)
    }

    def canConnectRedwire(gate:T, r:Int):Boolean = canConnectRedwire(gate.shape, r)
    def canConnectRedwire(shape:Int, r:Int):Boolean = (redwireMask(shape)&1<<r) != 0

    def redwireMask(shape:Int):Int

    def getConnType(r:Int):Int
    def getInputColourMask(r:Int):Int
    def getOutputColourMask(r:Int):Int
    def getPropMask(r:Int):Int
}

class ArrayGateICTile extends RedstoneGateICTile with TComplexGateICTile with TArrayGateICTile
{
    var logic:ArrayGateTileLogic = null

    override def assertLogic()
    {
        if (logic == null) logic = ArrayGateTileLogic.create(this, subID)
    }

    override def getLogic[T]:T = logic.asInstanceOf[T]

    override def getPartType = ICTileDefs.ArrayGate
}

object ArrayGateTileLogic
{
    import mrtjp.projectred.fabrication.{ICGateDefinition => defs}

    def create(gate:ArrayGateICTile, subID:Int):ArrayGateTileLogic = subID match
    {
        case defs.NullCell.ordinal => new NullCell(gate)
//        case defs.InvertCell.ordinal => new InvertCell(gate)
//        case defs.BufferCell.ordinal => new BufferCell(gate)
        case _ => throw new IllegalArgumentException("Invalid gate subID: "+subID)
    }
}

abstract class ArrayGateTileLogic(val gate:ArrayGateICTile) extends RedstoneGateTileLogic[ArrayGateICTile] with TComplexGateTileLogic[ArrayGateICTile] with TArrayGateTileLogic[ArrayGateICTile]

abstract class ArrayGateTileLogicCrossing(gate:ArrayGateICTile) extends ArrayGateTileLogic(gate)
{
    override def redwireMask(shape:Int) = 0xF

    override def getConnType(r:Int) = 0

    override def getInputColourMask(r:Int) = 0xFFFF
    override def getOutputColourMask(r:Int) = 0xFFFF

    override def getPropMask(r:Int) = if (r%2 == 0) 0x5 else 0xA

    val inputRegs = Array(-1, -1, -1, -1)
    val outputRegs = Array(-1, -1, -1, -1)
    val stateRegs = Array(-1, -1, -1, -1)

    def cacheIORegisters(linker:ISELinker)
    {
        for (r <- 0 until 4) {
            inputRegs(r) =
                    if (canInput(gate, r)) gate.getInputRegister(r, linker) else -1
            outputRegs(r) =
                    if (canOutput(gate, r)) gate.getOutputRegister(r, linker) else -1
            stateRegs(r) =
                    if (getPropMask(r) != 0) gate.getStateRegister(r, linker) else -1
        }
//
//        import SEIntegratedCircuit._
//        if (inputRegs.forall(id => id == -1 || id == REG_ZERO))
//            linker.getLogger.logWarning(Seq(gate.pos), "gate has no inputs")
//        if (outputRegs.forall(id => id == -1 || id == REG_ZERO))
//            linker.getLogger.logWarning(Seq(gate.pos), "gate has no outputs")
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

    private def pullWireState(mask:Int) = //Pull the raw wire state from the sim engine
    {
        var state = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0) {
            if (gate.editor.simEngineContainer.simEngine.getRegVal[Byte](stateRegs(r)) > 0) state |= 1<<r
        }
        state
    }

    def pullIOStateFromSim()
    {
        val oldState = gate.state
        val wireState = pullWireState(0xF)
        var newState = pullInput(inputMask(gate.shape))&0xF | pullOutput(outputMask(gate.shape))<<4
        newState |= wireState | wireState<<4

        if (oldState != newState) {
            gate.setState(newState)
            gate.sendStateUpdate()
        }
    }

    override def allocateOrFindRegisters(gate:ArrayGateICTile, linker:ISELinker)
    {
        cacheIORegisters(linker)
        allocInternalRegisters(linker)
    }

    override def onRegistersChanged(gate:ArrayGateICTile, regIDs:Set[Int])
    {
        pullIOStateFromSim()
    }

    def allocInternalRegisters(linker:ISELinker)
}

class NullCell(gate:ArrayGateICTile) extends ArrayGateTileLogicCrossing(gate)
{
    override def allocInternalRegisters(linker:ISELinker){}

    override def declareOperations(gate:ArrayGateICTile, linker:ISELinker){}
}

//abstract class ArrayGateTileLogic(gate:SequentialGateICTile) extends SequentialGateTileLogic(gate)
//{
//    override def outputMask(shape:Int):Int = 0xF
//    override def inputMask(shape:Int):Int = 0xF
//
//    override def allocInternalRegisters(linker:ISELinker){}
//
//    def declareBidirectionalFlow(mask:Int, linker:ISELinker)
//    {
//        def createFlowLogic(fromR:Int, toR:Int)
//        {
//            val inputReg = inputRegs(fromR)
//            val outputReg = outputRegs(toR)
//
//            val calculation = new ISEGate {
//                override def compute(ic:SEIntegratedCircuit) {
//                    ic.queueRegVal[Byte](outputReg, ic.getRegVal[Byte](inputReg))
//                }
//            }
//
//            val gateID = linker.allocateGateID(Set(gate.pos))
//            linker.addGate(gateID, calculation, Seq(inputReg), Seq(outputReg))
//        }
//
//        for (r0 <- 0 until 4) if ((mask&1<<r0) != 0)
//            for (r1 <- 0 until 4) if (r1 != r0 && (mask&1<<r1) != 0) {
//                createFlowLogic(r0, r1)
//            }
//    }
//}
//
//class NullCell(gate:SequentialGateICTile) extends ArrayGateTileLogic(gate)
//{
//    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
//    {
//        declareBidirectionalFlow(0x5, linker)
//        declareBidirectionalFlow(0xA, linker)
//    }
//}
//
//class InvertCell(gate:SequentialGateICTile) extends ArrayGateTileLogic(gate)
//{
//    def declareConditionalFlow(mask:Int, linker:ISELinker)
//    {
//        def createFlowLogic(fromR:Int, toR:Int)
//        {
//            val inputReg = inputRegs(fromR)
//            val outputReg = outputRegs(toR)
//
//            val conditionInputRegA = inputRegs(0)
//            val conditionInputRegB = inputRegs(2)
//
//            val calculation = new ISEGate {
//                override def compute(ic:SEIntegratedCircuit) {
//                    def lowerWirePowered = ic.getRegVal[Byte](conditionInputRegA) != 0 || ic.getRegVal[Byte](conditionInputRegB) != 0
//                    def cellPowerUp = !lowerWirePowered
//
//                    ic.queueRegVal[Byte](outputReg, if (cellPowerUp) 1 else ic.getRegVal[Byte](inputReg))
//                }
//            }
//
//            val gateID = linker.allocateGateID(Set(gate.pos))
//            linker.addGate(gateID, calculation, Seq(inputReg, conditionInputRegA, conditionInputRegB), Seq(outputReg))
//        }
//
//        for (r0 <- 0 until 4) if ((mask&1<<r0) != 0)
//            for (r1 <- 0 until 4) if (r1 != r0 && (mask&1<<r1) != 0) {
//                createFlowLogic(r0, r1)
//            }
//    }
//
//    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
//    {
//        declareBidirectionalFlow(0x5, linker)
//        declareConditionalFlow(0xA, linker)
//    }
//}
//
//class BufferCell(gate:SequentialGateICTile) extends ArrayGateTileLogic(gate)
//{
//    def declareConditionalFlow(mask:Int, linker:ISELinker)
//    {
//        def createFlowLogic(fromR:Int, toR:Int)
//        {
//            val inputReg = inputRegs(fromR)
//            val outputReg = outputRegs(toR)
//
//            val conditionInputRegA = inputRegs(0)
//            val conditionInputRegB = inputRegs(2)
//
//            val calculation = new ISEGate {
//                override def compute(ic:SEIntegratedCircuit) {
//                    def lowerWirePowered = ic.getRegVal[Byte](conditionInputRegA) != 0 || ic.getRegVal[Byte](conditionInputRegB) != 0
//                    def cellPowerUp = lowerWirePowered
//
//                    ic.queueRegVal[Byte](outputReg, if (cellPowerUp) 1 else ic.getRegVal[Byte](inputReg))
//                }
//            }
//
//            val gateID = linker.allocateGateID(Set(gate.pos))
//            linker.addGate(gateID, calculation, Seq(inputReg, conditionInputRegA, conditionInputRegB), Seq(outputReg))
//        }
//
//        for (r0 <- 0 until 4) if ((mask&1<<r0) != 0)
//            for (r1 <- 0 until 4) if (r1 != r0 && (mask&1<<r1) != 0) {
//                createFlowLogic(r0, r1)
//            }
//    }
//
//    override def declareOperations(gate:SequentialGateICTile, linker:ISELinker)
//    {
//        declareBidirectionalFlow(0x5, linker)
//        declareConditionalFlow(0xA, linker)
//    }
//
//}