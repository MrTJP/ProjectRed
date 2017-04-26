package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.ProjectRedCore.log
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{NBTTagCompound, NBTTagList}
import net.minecraft.world.World

import scala.collection.mutable.{Map => MMap}

trait IICTileEditorNetwork
{
    def getIC:ICTileMapEditor
    def getWorld:World

    def getICStreamOf(key:Int):MCDataOutput
    def getPartStream(x:Int, y:Int):MCDataOutput

    def isRemote:Boolean
    def markSave()
}

trait TICTileEditorNetwork extends IICTileEditorNetwork
{
    private var icStream:PacketCustom = null
    private var partStream:PacketCustom = null

    def createPartStream():PacketCustom
    def sendPartStream(out:PacketCustom)
    override def getPartStream(x:Int, y:Int):MCDataOutput =
    {
        if (partStream == null) partStream = createPartStream()

        val part = getIC.getPart(x, y)
        partStream.writeByte(part.id)
        partStream.writeByte(x).writeByte(y)

        partStream
    }
    def flushPartStream()
    {
        if (partStream != null)
        {
            partStream.writeByte(255)//terminator
            sendPartStream(partStream.compress())
            partStream = null
        }
    }
    def readPartStream(in:MCDataInput)
    {
        try {
            var id = in.readUByte()
            while (id != 255) {
                val (x, y) = (in.readUByte(), in.readUByte())
                var part = getIC.getPart(x, y)
                if (part == null || part.id != id) {
                    log.error("client part stream couldnt find part "+Point(x, y))
                    part = ICTile.createTile(id)
                }
                part.read(in)
                id = in.readUByte()
            }
        }
        catch {
            case ex:IndexOutOfBoundsException =>
                log.error("Circuit part stream failed to be read.")
                ex.printStackTrace()
        }
    }

    def createICStream():PacketCustom
    def sendICStream(out:PacketCustom)
    override def getICStreamOf(key:Int):MCDataOutput =
    {
        if (icStream == null) icStream = createICStream()
        icStream.writeByte(key)
        icStream
    }
    def flushICStream()
    {
        if (icStream != null)
        {
            icStream.writeByte(255) //terminator
            sendICStream(icStream.compress())
            icStream = null
        }
    }
    def readICStream(in:MCDataInput)
    {
        try {
            var id = in.readUByte()
            while (id != 255) {
                getIC.read(in, id)
                id = in.readUByte()
            }
        } catch {
            case ex:IndexOutOfBoundsException =>
                log.error("Tile Map stream failed to be read")
        }
    }
}

class ICTileMapContainer extends ISETileMap
{
    override val tiles = MMap[(Int, Int), ICTile]()

    var tilesLoadedDelegate = {() => ()}

    var name = "untitled"

    var size = Size.zeroSize

    def isEmpty = size == Size.zeroSize

    def nonEmpty = !isEmpty

    def assertCoords(x:Int, y:Int)
    {
        if (!(0 until size.width contains x) || !(0 until size.height contains y))
            throw new IndexOutOfBoundsException("Tile Map does not contain "+Point(x, y))
    }

    def saveTiles(tag:NBTTagCompound)
    {
        tag.setString("name", name)
        tag.setByte("sw", size.width.toByte)
        tag.setByte("sh", size.height.toByte)

        val tagList = new NBTTagList
        for (part <- tiles.values) {
            val partTag = new NBTTagCompound
            partTag.setByte("id", part.id.toByte)
            partTag.setByte("xpos", part.x.toByte)
            partTag.setByte("ypos", part.y.toByte)
            part.save(partTag)
            tagList.appendTag(partTag)
        }
        tag.setTag("parts", tagList)
    }

    def loadTiles(tag:NBTTagCompound)
    {
        name = tag.getString("name")
        size = Size(tag.getByte("sw")&0xFF, tag.getByte("sh")&0xFF)

        val partList = tag.getTagList("parts", 10)
        for(i <- 0 until partList.tagCount) {
            val partTag = partList.getCompoundTagAt(i)
            val part = ICTile.createTile(partTag.getByte("id")&0xFF)
            val x = partTag.getByte("xpos")&0xFF
            val y = partTag.getByte("ypos")&0xFF
            part.bindPos(x, y)
            tiles += (x, y) -> part
            part.load(partTag)
        }

        tilesLoadedDelegate()
    }
}

class ICTileMapEditor(val network:IICTileEditorNetwork)
{
    val tileMapContainer = new ICTileMapContainer

    var simEngineContainer = new ICSimEngineContainer
    var simNeedsRefresh = true

    var lastWorldTime = -1L

    var errors = Map.empty[Point, (String, Int)]

    private var scheduledTicks = MMap[(Int, Int), Long]()

    tileMapContainer.tilesLoadedDelegate = {() =>
        simNeedsRefresh = true
        for (part <- tileMapContainer.tiles.values)
            part.bindEditor(this)
    }

    def size = tileMapContainer.size
    def name = tileMapContainer.name

    def save(tag:NBTTagCompound)
    {
        tileMapContainer.saveTiles(tag)
        simEngineContainer.saveSimState(tag)
    }

    def load(tag:NBTTagCompound)
    {
        clear()
        tileMapContainer.loadTiles(tag)

        simEngineContainer.propagateSilently = true
        recompileSchematic()
        simEngineContainer.loadSimState(tag)
        simEngineContainer.propagateSilently = false
    }

    def writeDesc(out:MCDataOutput)
    {
        out.writeString(tileMapContainer.name)
        out.writeByte(tileMapContainer.size.width).writeByte(tileMapContainer.size.height)
        for (i <- 0 until 4) out.writeInt(simEngineContainer.iostate(i))

        for (((x, y), part) <- tileMapContainer.tiles) {
            out.writeByte(part.id)
            out.writeByte(x).writeByte(y)
            part.writeDesc(out)
        }
        out.writeByte(255)
    }

    def readDesc(in:MCDataInput)
    {
        clear()
        tileMapContainer.name = in.readString()
        tileMapContainer.size = Size(in.readUByte(), in.readUByte())
        for (i <- 0 until 4) simEngineContainer.iostate(i) = in.readInt()

        var id = in.readUByte()
        while (id != 255) {
            val part = ICTile.createTile(id)
            setPart_do(in.readUByte(), in.readUByte(), part)
            part.readDesc(in)
            id = in.readUByte()
        }
    }

    def read(in:MCDataInput, key:Int) = key match
    {
        case 0 => readDesc(in)
        case 1 =>
            val part = ICTile.createTile(in.readUByte())
            setPart_do(in.readUByte(), in.readUByte(), part)
            part.readDesc(in)
        case 2 => removePart(in.readUByte(), in.readUByte())
        case 3 => TileEditorOp.getOperation(in.readUByte()).readOp(this, in)
        case 4 => getPart(in.readUByte(), in.readUByte()) match {
            case g:TClientNetICTile => g.readClientPacket(in)
            case _ => log.error("Server IC stream received invalid client packet")
        }
        case 5 =>
            for (r <- 0 until 4)
                simEngineContainer.iostate(r) = in.readInt()
        case 6 => simEngineContainer.setInput(in.readUByte(), in.readShort())//TODO remove? not used...
        case 7 => simEngineContainer.setOutput(in.readUByte(), in.readShort()) //TODO remove? not used...
        case _ =>
    }

    def sendPartAdded(part:ICTile)
    {
        val out = network.getICStreamOf(1)
        out.writeByte(part.id)
        out.writeByte(part.x).writeByte(part.y)
        part.writeDesc(out)
    }

    def sendRemovePart(x:Int, y:Int)
    {
        network.getICStreamOf(2).writeByte(x).writeByte(y)
    }

    def sendOpUse(op:TileEditorOp, start:Point, end:Point) =
    {
        if (op.checkOp(this, start, end)) {
            op.writeOp(this, start, end, network.getICStreamOf(3).writeByte(op.id))
            true
        }
        else false
    }

    def sendClientPacket(part:TClientNetICTile, writer:MCDataOutput => Unit)
    {
        val s = network.getICStreamOf(4).writeByte(part.x).writeByte(part.y)
        writer(s)
    }

    def sendIOUpdate()
    {
        val stream = network.getICStreamOf(5)
            for (r <- 0 until 4)
                stream.writeInt(simEngineContainer.iostate(r))
    }

    def sendInputUpdate(r:Int) //TODO Remove?
    {
        network.getICStreamOf(6).writeByte(r).writeShort(simEngineContainer.iostate(r)&0xFFFF)
    }

    def sendOutputUpdate(r:Int) //TODO Remove?
    {
        network.getICStreamOf(7).writeByte(r).writeShort(simEngineContainer.iostate(r)>>>16)
    }

    def clear()
    {
        tileMapContainer.tiles.values.foreach{_.unbind()}//remove references
        tileMapContainer.tiles.clear()
        scheduledTicks = MMap()
        tileMapContainer.name = "untitled"
        tileMapContainer.size = Size.zeroSize
        for (i <- 0 until 4) simEngineContainer.iostate(i) = 0
        simNeedsRefresh = true
    }

    def isEmpty = tileMapContainer.isEmpty
    def nonEmpty = tileMapContainer.nonEmpty

    def tick()
    {
        //Update parts as needed
        val t = network.getWorld.getTotalWorldTime
        var rem = Seq[(Int, Int)]()
        for((k, v) <- scheduledTicks) if(v >= t) {
            getPart(k._1, k._2).scheduledTick()
            rem :+= k
        }
        rem.foreach(scheduledTicks.remove)

        //Tick parts
        for(part <- tileMapContainer.tiles.values) part.update()

        //Rebuild circuit if needed
        if (simNeedsRefresh)
            recompileSchematic()

        //Tick Simulation time
        simEngineContainer.advanceTime(if (lastWorldTime >= 0) t-lastWorldTime else 1) //if first tick, advance 1 tick only
        simEngineContainer.repropagate()
        lastWorldTime = t
    }

    def refreshErrors()
    {
        val eparts = tileMapContainer.tiles.values.collect {case p:IErrorICTile => p}
        val elist = Map.newBuilder[Point, (String, Int)]

        for (part <- eparts) {
            val error = part.postErrors
            if (error != null)
                elist += Point(part.x, part.y) -> error
        }

        errors = elist.result()
    }

    def setPart(x:Int, y:Int, part:ICTile)
    {
        setPart_do(x, y, part)
        part.onAdded()
        if (!network.isRemote) {
            sendPartAdded(part)
            markSchematicChanged()
        }
    }
    private def setPart_do(x:Int, y:Int, part:ICTile)
    {
        tileMapContainer.assertCoords(x, y)
        part.bindEditor(this)
        part.bindPos(x, y)
        tileMapContainer.tiles += (x, y) -> part
    }

    def getPart(x:Int, y:Int):ICTile = tileMapContainer.tiles.getOrElse((x, y), null)

    def removePart(x:Int, y:Int)
    {
        tileMapContainer.assertCoords(x, y)
        val part = getPart(x, y)
        if (part != null) {
            if (!network.isRemote) {
                sendRemovePart(x, y)
                markSchematicChanged()
            }
            tileMapContainer.tiles.remove((x, y))
            part.onRemoved()
            part.unbind()
        }
    }

    def notifyNeighbor(x:Int, y:Int)
    {
        val part = getPart(x, y)
        if (part != null) part.onNeighborChanged()
    }

    def notifyNeighbors(x:Int, y:Int, mask:Int)
    {
        for(r <- 0 until 4) if ((mask&1<<r) != 0) {
            val point = Point(x, y).offset(r)
            val part = getPart(point.x, point.y)
            if (part != null) part.onNeighborChanged()
        }
    }

    def scheduleTick(x:Int, y:Int, ticks:Int){scheduledTicks += (x, y) -> (network.getWorld.getTotalWorldTime+ticks)}

    def markSchematicChanged()
    {
        simNeedsRefresh = true
    }

    def recompileSchematic()
    {
        simNeedsRefresh = false
        simEngineContainer.registersChangedDelegate = onICSimFinished
        simEngineContainer.ioChangedDelegate = sendIOUpdate
        simEngineContainer.recompileSimulation(tileMapContainer)
        simEngineContainer.propagateAll()
    }

    def onICSimFinished(changedRegs:Set[Int])
    {
        for (part <- tileMapContainer.tiles.values)
            part.onRegistersChanged(changedRegs)
    }

    //Convinience functions
    def setPart(p:Point, part:ICTile){setPart(p.x, p.y, part)}
    def getPart(p:Point):ICTile = getPart(p.x, p.y)
    def removePart(p:Point){removePart(p.x, p.y)}
    def notifyNeighbor(p:Point){notifyNeighbor(p.x, p.y)}
    def notifyNeighbors(p:Point, mask:Int){notifyNeighbors(p.x, p.y, mask)}
    def scheduleTick(p:Point, ticks:Int){scheduleTick(p.x, p.y, ticks)}
}