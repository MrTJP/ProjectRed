package mrtjp.projectred.fabrication

import mrtjp.core.vec.Point

import scala.collection.mutable.{ListBuffer, Map => MMap}

trait ISETile

trait ISEWireTile extends ISETile
{
    def buildWireNet(r:Int):IWireNet

    def cacheStateRegisters(linker:ISELinker)
}

trait ISEGateTile extends ISETile
{
    def buildImplicitWireNet(r:Int):IWireNet

    def allocateOrFindRegisters(linker:ISELinker)

    def declareOperations(linker:ISELinker)
}

trait IWireNet
{
    val points:scala.collection.mutable.Seq[(Point, Int)]

    def allocateRegisters(linker:ISELinker)

    def declareOperations(linker:ISELinker)

    def getInputRegister(p:Point, r:Int):Int

    def getOutputRegister(p:Point, r:Int):Int
}

trait ISETileMap
{
    val tiles:scala.collection.Map[(Int, Int), ISETile]
}

trait ISEStatLogger
{
    def clear()

    def logInfo(message:String)

    def logWarning(points:Seq[Point], message:String)

    def logError(points:Seq[Point], message:String)

    def logRuntimeFlag(flag:Int, registers:Seq[Int], gates:Seq[Int])

    def logRegAlloc(id:Int, points:Set[Point])

    def logGateAlloc(id:Int, points:Set[Point])
}

trait ISELinker
{
    /**
      * Allocates space for a register.
      *
      * @param associatedPoints The tile map points associated with this register. Used
      *                         for runtime debugging/logging purposes
      * @return The ID where this register can be added
      */
    def allocateRegisterID(associatedPoints:Set[Point]):Int

    /**
      * Allocates space for a gate
      *
      * @param associatedPoints The tile map points associated with this gate. Used
      *                         for runtime debugging/logging purposes
      * @return The ID where this gate can be added
      */
    def allocateGateID(associatedPoints:Set[Point]):Int

    /**
      * Sets the ID to the given register
      * @param id A previously allocated register ID
      * @param r The register to put in the slot
      */
    def addRegister(id:Int, r:ISERegister)

    /**
      * Sets the ID to the given gate
      * @param id A previously allocated gate ID
      * @param g The gate
      * @param drivingRegs Registers responsible for driving the gate
      * @param drivenRegs Registers that will be driven by the gate
      */
    def addGate(id:Int, g:ISEGate, drivingRegs:Seq[Int], drivenRegs:Seq[Int])

    /**
      * Callback used by tiles during the linking process. Used to locate a
      * register inputting to point `p` from direction `r`
      *
      * @return The register ID of the located register, or a zero-constant register
      *         if none found.
      */
    def findInputRegister(p:Point, r:Int):Int

    /**
      * Callback used by tiles during the linking process. Used to locate a
      * register outputting from point `p` to direction `r`
      *
      * @return The register ID of the located register, or a zero-constant register
      *         if none found.
      */
    def findOutputRegister(p:Point, r:Int):Int

    /**
      * Callback used by tiles during the linking process. Used to locate a
      * register of a wirenet on point `p` passing through direction `r`.
      *
      * @return The register ID of the located register, or a zero-constant register
      *         if none found.
      */
    def getWirenetOutputRegister(p:Point, r:Int):Int

    /**
      * Get the logger for the current compilation. Used during the compilations
      * to record note-worthy information as well as statistics, errors, warnings,
      * etc.
      *
      * @return The logger object for the current compilation
      */
    def getLogger:ISEStatLogger
}

object ISELinker
{
    def linkFromMap(map:ISETileMap, logger:ISEStatLogger):SEIntegratedCircuit =
        SELinker.linkFromMap(map, logger)
}

private class SELinker(logger:ISEStatLogger) extends ISELinker
{
    import SEIntegratedCircuit._

    private var registers = Array[ISERegister]() //The registers currently in the circuit, indexed by ID
    private var gates = Array[ISEGate]() //The gates in the circuit indexed by ID

    private var regDependents = MMap[Int, Seq[Int]]() //Register deps [regID -> Seq[gateID]]
    private var regDependencies = MMap[Int, Seq[Int]]() //Register depenendices [regID -> Seq[gateID]]
    private var gateDependents = MMap[Int, Seq[Int]]() //[gateID -> Seq[regID]]
    private var gateDependencies = MMap[Int, Seq[Int]]() //[gateID -> Seq[regID]]

    private val wireNets = new ListBuffer[IWireNet]()
    private val implicitWireNets = new ListBuffer[IWireNet]()
    private val wireNetMap = MMap[(Point, Int), IWireNet]() //Wire register map [(pos, rotation) -> net]

    /*
     * Stores implicit wires such as between two touching gates. There
     * is no physical wire on the map, but there needs to be one for proper
     * propagation. The map works as:
     *
     * [Set(p1, p2) -> (regID, wireType)]
     *
     * where p1 and p2 are adjacent points on the map
     */
    private val implicitWireNetMap = MMap[Set[Point], IWireNet]()

    private var registerIDPool = 0
    private var gateIDPool = 0

    override def allocateRegisterID(associatedPoints:Set[Point]) =
    {
        val id = registerIDPool
        logger.logRegAlloc(id, associatedPoints)
        registerIDPool += 1
        id
    }

    override def allocateGateID(associatedPoints:Set[Point]) =
    {
        val id = gateIDPool
        logger.logGateAlloc(id, associatedPoints)
        gateIDPool += 1
        id
    }

    override def addRegister(id:Int, r:ISERegister)
    {
        while (registers.length <= id)
            registers :+= null
        registers(id) = r
    }

    override def addGate(id:Int, g:ISEGate, drivingRegs:Seq[Int], drivenRegs:Seq[Int])
    {
        while (gates.length <= id)
            gates :+= null
        gates(id) = g

        for (regID <- drivingRegs) {
            val others = regDependents.getOrElse(regID, Seq.empty)
            regDependents += regID -> (others :+ id)
        }

        for (regID <- drivenRegs) {
            val others = regDependencies.getOrElse(regID, Seq.empty)
            regDependencies += regID -> (others :+ id)
        }

        gateDependents += id -> drivingRegs
        gateDependencies += id -> drivingRegs
    }

    override def findInputRegister(p:Point, r:Int):Int = //register that inputs to p from side r
    {
        val p2 = p.offset(r)
        val r2 = (r+2)%4
        wireNetMap.get(p2, r2) match {
            case Some(net) => net.getOutputRegister(p2, r2) //Input to p is output from p2
            case _ => implicitWireNetMap.get(Set(p, p2)) match {
                case Some(net) => net.getOutputRegister(p2, r2)
                case _ => REG_ZERO
            }
        }
    }

    override def findOutputRegister(p:Point, r:Int):Int = //register that outputs from p to side r
    {
        val p2 = p.offset(r)
        val r2 = (r+2)%4
        wireNetMap.get((p2, r2)) match {
            case Some(net) => net.getInputRegister(p2, r2) //Output to p is input from p2
            case _ => implicitWireNetMap.get(Set(p, p2)) match {
                case Some(net) => net.getInputRegister(p2, r2)
                case _ => REG_ZERO
            }
        }
    }

    override def getWirenetOutputRegister(p:Point, r:Int):Int = //wirenet output reg on point p side r
    {
        wireNetMap.get((p, r)) match {
            case Some(net) => net.getOutputRegister(p, r)
            case _ =>
                val p2 = p.offset(r)
                implicitWireNetMap.get(Set(p, p2)) match {
                    case Some(net) => net.getOutputRegister(p, r)
                    case _ => REG_ZERO
                }
        }
    }

    override def getLogger = logger
}

private object SELinker
{
    def linkFromMap(map:ISETileMap, logger:ISEStatLogger):SEIntegratedCircuit =
    {
        val linker = new SELinker(logger)
        import linker._

        val startTime = System.currentTimeMillis()

        logger.logInfo("Adding SFRs...")
        //Add all SFRs
        import SEIntegratedCircuit._
        for (r <- 0 until 4) for (i <- 0 until 16)
            addRegister(REG_IN(r, i), new StandardRegister[Byte](0))
        for (r <- 0 until 4) for (i <- 0 until 16)
            addRegister(REG_OUT(r, i), new StandardRegister[Byte](0))
        addRegister(REG_SYSTIME, new StandardRegister[Long](0L))
        addRegister(REG_ZERO, new ConstantRegister[Byte](0))
        addRegister(REG_ONE, new ConstantRegister[Byte](1))

        registerIDPool = REG_ONE+1

        val allWires = map.tiles.collect {
            case ((x, y), w:ISEWireTile) => (Point(x, y), w)
        }
        val allGates = map.tiles.collect {
            case ((x, y), g:ISEGateTile) => (Point(x, y), g)
        }

        logger.logInfo("Creating wirenets...")
        // Register all wire networks
        for ((p, w) <- allWires) { //Start with normal wire nets. Ask each wire to assemble a network.
            for (r <- 0 until 4) {
                if (!wireNetMap.contains((p, r))) {
                    val net = w.buildWireNet(r) //Assemble it...
                    if (net != null) {
                        wireNets += net //Store it...
                        for ((p, r) <- net.points)
                            wireNetMap += (p, r) -> net //And map it...
                    }

                    logger.logInfo(s"Added wirenet originating at $p on side $r")
                }
            }

//            if (!wireNetMap.contains(p)) { //If the net has not been created yet by another wire in the net...
//                logger.logInfo(s"Added wirenet originating at $p")
//                val net = w.buildWireNet //Assemble it...
//                wireNets += net //Store it...
//                for (netP <- net.points)
//                    wireNetMap += netP -> net //And map it...
//            }
        }

        logger.logInfo("Creating implicit wirenets...")
        for ((p, g) <- allGates) { //Then add implicit nets. These are wires between adjacent gates.
            for (r <- 0 until 4) {
                val p2 = p.offset(r)
                val pSet = Set(p, p2)
                if (!implicitWireNetMap.contains(pSet)) {
                    val net = g.buildImplicitWireNet(r)
                    if (net != null) {
                        implicitWireNets += net
                        implicitWireNetMap += pSet -> net
                        logger.logInfo(s"Added implicit wirenet for $p -> $p2")
                    }
                }
            }
        }

        logger.logInfo("Allocating wirenet registers...")
        // Add required registers from all parts
        for (net <- wireNets ++ implicitWireNets)
            net.allocateRegisters(linker) //from wires
        for ((p, w) <- allWires)
            w.cacheStateRegisters(linker) //tell wires about the registers

        logger.logInfo("Allocating gate registers...")
        for ((_, g) <- allGates)
            g.allocateOrFindRegisters(linker) //from gates

        logger.logInfo("Declaring operations for wires...")
        // Add all gate operations
        for (net <- wireNets ++ implicitWireNets)
            net.declareOperations(linker) //from wires

        logger.logInfo("Declaring operations for gates...")
        for ((_, g) <- allGates)
            g.declareOperations(linker) //from gates

        logger.logInfo("Creating IC...")
        val ic = new SEIntegratedCircuit(
            registers.toSeq,
            gates.toSeq,
            regDependents.toMap
        )

        logger.logInfo("Computing initial signals...")
        ic.computeAll()

        val endTime = System.currentTimeMillis()

        logger.logInfo(s"Completed linking in ${endTime-startTime} ms.")
        ic
    }
}