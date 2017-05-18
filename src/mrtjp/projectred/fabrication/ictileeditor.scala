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
    def getTileStream(x:Int, y:Int):MCDataOutput

    def isRemote:Boolean
    def markSave()
}

trait TICTileEditorNetwork extends IICTileEditorNetwork
{
    private var editorStream:PacketCustom = null
    private var tileStream:PacketCustom = null

    def createTileStream():PacketCustom
    def sendTileStream(out:PacketCustom)
    override def getTileStream(x:Int, y:Int):MCDataOutput =
    {
        if (tileStream == null) tileStream = createTileStream()

        val tile = getIC.getTile(x, y)
        tileStream.writeByte(tile.id)
        tileStream.writeByte(x).writeByte(y)

        tileStream
    }
    def flushTileStream()
    {
        if (tileStream != null) {
            tileStream.writeByte(255)//terminator
            sendTileStream(tileStream.compress())
            tileStream = null
        }
    }
    def readTileStream(in:MCDataInput)
    {
        try {
            var id = in.readUByte()
            while (id != 255) {
                val (x, y) = (in.readUByte(), in.readUByte())
                var tile = getIC.getTile(x, y)
                if (tile == null || tile.id != id) {
                    log.error("client tile stream couldnt find tile "+Point(x, y))
                    tile = ICTile.createTile(id)
                }
                tile.read(in)
                id = in.readUByte()
            }
        }
        catch {
            case ex:IndexOutOfBoundsException =>
                log.error("tile stream failed to be read.")
                ex.printStackTrace()
        }
    }

    def createEditorStream():PacketCustom
    def sendEditorStream(out:PacketCustom)

    override def getICStreamOf(key:Int):MCDataOutput =
    {
        if (editorStream == null) editorStream = createEditorStream()
        editorStream.writeByte(key)
        editorStream
    }
    def flushICStream()
    {
        if (editorStream != null) {
            editorStream.writeByte(255) //terminator
            sendEditorStream(editorStream.compress())
            editorStream = null
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
        for (tile <- tiles.values) {
            val tileTag = new NBTTagCompound
            tileTag.setByte("id", tile.id.toByte)
            tileTag.setByte("xpos", tile.x.toByte)
            tileTag.setByte("ypos", tile.y.toByte)
            tile.save(tileTag)
            tagList.appendTag(tileTag)
        }
        tag.setTag("tiles", tagList)
    }

    def loadTiles(tag:NBTTagCompound)
    {
        name = tag.getString("name")
        size = Size(tag.getByte("sw")&0xFF, tag.getByte("sh")&0xFF)

        val tileList = tag.getTagList("tiles", 10)
        for(i <- 0 until tileList.tagCount) {
            val tileTag = tileList.getCompoundTagAt(i)
            val tile = ICTile.createTile(tileTag.getByte("id")&0xFF)
            val x = tileTag.getByte("xpos")&0xFF
            val y = tileTag.getByte("ypos")&0xFF
            tile.bindTileMap(this)
            tile.bindPos(x, y)
            tiles += (x, y) -> tile
            tile.load(tileTag)
        }

        tilesLoadedDelegate()
    }

    def getTile(x:Int, y:Int):ICTile = tiles.getOrElse((x, y), null)

    def getTile(p:Point):ICTile = getTile(p.x, p.y)
}

class ICTileMapEditor(val network:IICTileEditorNetwork)
{
    val tileMapContainer = new ICTileMapContainer

    var simEngineContainer = new ICSimEngineContainer
    var simNeedsRefresh = true

    var lastWorldTime = -1L

    private var scheduledTicks = MMap[(Int, Int), Long]()

    tileMapContainer.tilesLoadedDelegate = {() =>
        simNeedsRefresh = true
        for (tile <- tileMapContainer.tiles.values)
            tile.bindEditor(this)
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
        simEngineContainer.logger.writeLog(out)

        for (((x, y), tile) <- tileMapContainer.tiles) {
            out.writeByte(tile.id)
            out.writeByte(x).writeByte(y)
            tile.writeDesc(out)
        }
        out.writeByte(255)
    }

    def readDesc(in:MCDataInput)
    {
        clear()
        tileMapContainer.name = in.readString()
        tileMapContainer.size = Size(in.readUByte(), in.readUByte())
        for (i <- 0 until 4) simEngineContainer.iostate(i) = in.readInt()
        simEngineContainer.logger.readLog(in)

        var id = in.readUByte()
        while (id != 255) {
            val tile = ICTile.createTile(id)
            setTile_do(in.readUByte(), in.readUByte(), tile)
            tile.readDesc(in)
            id = in.readUByte()
        }
    }

    def read(in:MCDataInput, key:Int) = key match
    {
        case 0 => readDesc(in)
        case 1 =>
            val tile = ICTile.createTile(in.readUByte())
            setTile_do(in.readUByte(), in.readUByte(), tile)
            tile.readDesc(in)
        case 2 => removeTile(in.readUByte(), in.readUByte())
        case 3 => TileEditorOp.getOperation(in.readUByte()).readOp(this, in)
        case 4 => getTile(in.readUByte(), in.readUByte()) match {
            case g:TClientNetICTile => g.readClientPacket(in)
            case _ => log.error("Server IC stream received invalid client packet")
        }
        case 5 =>
            for (r <- 0 until 4)
                simEngineContainer.iostate(r) = in.readInt()
        case 6 => simEngineContainer.setInput(in.readUByte(), in.readShort())//TODO remove? not used...
        case 7 => simEngineContainer.setOutput(in.readUByte(), in.readShort()) //TODO remove? not used...
        case 8 => simEngineContainer.logger.readLog(in)
        case _ =>
    }

    def sendTileAdded(tile:ICTile)
    {
        val out = network.getICStreamOf(1)
        out.writeByte(tile.id)
        out.writeByte(tile.x).writeByte(tile.y)
        tile.writeDesc(out)
    }

    def sendRemoveTile(x:Int, y:Int)
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

    def sendClientPacket(tile:TClientNetICTile, writer:MCDataOutput => Unit)
    {
        val s = network.getICStreamOf(4).writeByte(tile.x).writeByte(tile.y)
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

    def sendCompileLog()
    {
        simEngineContainer.logger.writeLog(network.getICStreamOf(8))
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
        //Update tiles as needed
        val t = network.getWorld.getTotalWorldTime
        var rem = Seq[(Int, Int)]()
        for((k, v) <- scheduledTicks) if(v >= t) {
            getTile(k._1, k._2).scheduledTick()
            rem :+= k
        }
        rem.foreach(scheduledTicks.remove)

        //Tick tiles
        for(tile <- tileMapContainer.tiles.values) tile.update()

        //Rebuild circuit if needed
        if (simNeedsRefresh) {
            recompileSchematic()
            sendCompileLog()
        }

        //Tick Simulation time
        simEngineContainer.advanceTime(if (lastWorldTime >= 0) t-lastWorldTime else 1) //if first tick, advance 1 tick only
        simEngineContainer.repropagate()
        lastWorldTime = t
    }

    def setTile(x:Int, y:Int, tile:ICTile)
    {
        setTile_do(x, y, tile)
        tile.onAdded()
        if (!network.isRemote) {
            sendTileAdded(tile)
            markSchematicChanged()
        }
    }
    private def setTile_do(x:Int, y:Int, tile:ICTile)
    {
        tileMapContainer.assertCoords(x, y)
        tile.bindEditor(this)
        tile.bindPos(x, y)
        tileMapContainer.tiles += (x, y) -> tile
    }

    def getTile(x:Int, y:Int):ICTile = tileMapContainer.getTile(x, y)

    def removeTile(x:Int, y:Int)
    {
        tileMapContainer.assertCoords(x, y)
        val tile = getTile(x, y)
        if (tile != null) {
            if (!network.isRemote) {
                sendRemoveTile(x, y)
                markSchematicChanged()
            }
            tileMapContainer.tiles.remove((x, y))
            tile.onRemoved()
            tile.unbind()
        }
    }

    def notifyNeighbor(x:Int, y:Int)
    {
        val tile = getTile(x, y)
        if (tile != null) tile.onNeighborChanged()
    }

    def notifyNeighbors(x:Int, y:Int, mask:Int)
    {
        for(r <- 0 until 4) if ((mask&1<<r) != 0) {
            val point = Point(x, y).offset(r)
            val tile = getTile(point.x, point.y)
            if (tile != null) tile.onNeighborChanged()
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
        for (tile <- tileMapContainer.tiles.values)
            tile.onRegistersChanged(changedRegs)
    }

    //Convinience functions
    def setTile(p:Point, tile:ICTile){setTile(p.x, p.y, tile)}
    def getTile(p:Point):ICTile = getTile(p.x, p.y)
    def removeTile(p:Point){removeTile(p.x, p.y)}
    def notifyNeighbor(p:Point){notifyNeighbor(p.x, p.y)}
    def notifyNeighbors(p:Point, mask:Int){notifyNeighbors(p.x, p.y, mask)}
    def scheduleTick(p:Point, ticks:Int){scheduleTick(p.x, p.y, ticks)}
}