package mrtjp.projectred.core

import codechicken.lib.data.{MCDataByteBuf, MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.VoxelShapeCache
import codechicken.lib.vec.{Cuboid6, Rotation, Transformation, Vector3}
import mrtjp.core.world.WorldLib
import net.minecraft.block.BlockState
import net.minecraft.client.world.ClientWorld
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.{Entity, LivingEntity}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.tileentity.{ITickableTileEntity, TileEntity, TileEntityType}
import net.minecraft.util.math.shapes.VoxelShape
import net.minecraft.util.math.{BlockPos, BlockRayTraceResult}
import net.minecraft.util.{ActionResultType, Hand}
import net.minecraft.world.Explosion

import scala.collection.mutable.ListBuffer

/**
 * Base tile class on which usefull common traits can be applied onto.
 */
abstract class CoreTile(tileType:TileEntityType[_]) extends TileEntity(tileType) with ITickableTileEntity {
    protected var schedTick:Long = -1L

    def onBlockPlaced(player:LivingEntity, stack:ItemStack):Unit = {}
    def onBlockStateReplaced(newState:BlockState):Unit = {}
    def onBlockRemoved():Unit = {}

    def loadBlockState(state:BlockState):Unit = {}
    def covertToBlockState(state:BlockState):BlockState = state

    def getOutlineShape:VoxelShape = VoxelShapeCache.getShape(Cuboid6.full)
    def getCollisionShape:VoxelShape = getOutlineShape
    def getCullingShape:VoxelShape = getOutlineShape

    def getRayTraceShape:VoxelShape = getOutlineShape

    def getExplosionResistance(exploder:Entity, explosion:Explosion):Float = 0

    def getLightValue:Int = 0

    def getPlayerRelativeBlockHardness(player:PlayerEntity):Float = 1 / 30F

    def harvestTile(player:PlayerEntity):Unit = {}

    def getDrops:List[ItemStack] = List.empty

    def getPickBlock:ItemStack = ItemStack.EMPTY

    def onBlockActivated(player:PlayerEntity, hand:Hand, hit:BlockRayTraceResult):ActionResultType = ActionResultType.PASS
    def onBlockClicked(player:PlayerEntity):Unit = {}

    def onEntityCollision(entity:Entity):Unit = {}
    def onEntityWalk(entity:Entity):Unit = {}

    def onNeighborBlockChanged(neighborPos:BlockPos):Unit = {}
    def onNeighborTileChange(neighborPos:BlockPos):Unit = {}

    def getWeakChanges:Boolean = false //true if listening to weak changes

    def canConnectRedstone(side:Int):Boolean = false

    def getStrongPower(side:Int):Int = 0
    def getWeakPower(side:Int):Int = 0

    def addHarvestContents(ist: ListBuffer[ItemStack]):Unit = {
        ist += getPickBlock
    }

    final def dropAndRemove():Unit = {
        val il = new ListBuffer[ItemStack]
        addHarvestContents(il)
        for (stack <- il) WorldLib.dropItem(level, getBlockPos, stack)
        level.removeBlock(getBlockPos, false)
    }

    final override def setRemoved():Unit = {
        if (!isRemoved) {
            super.setRemoved()
            onBlockRemoved()
        }
    }

    final def pushState():Unit = {
        if (!level.isClientSide) {
            val baseState = getBlockState.getBlock.defaultBlockState()
            val newState = covertToBlockState(baseState)
            level.setBlockAndUpdate(getBlockPos, newState)
        }
    }

    final def markRender():Unit = {
//        world.markBlockRangeForRenderUpdate(pos, pos)
        if (level.isClientSide && level.isInstanceOf[ClientWorld]) {
            level.asInstanceOf[ClientWorld].levelRenderer
                    .setBlocksDirty(getBlockPos.getX, getBlockPos.getY, getBlockPos.getZ,
                        getBlockPos.getX, getBlockPos.getY, getBlockPos.getZ)
        }
    }

    final def recalcLight(sky:Boolean, block:Boolean):Unit = {
        val lm = level.getChunkSource.getLightEngine
        if (sky && lm.skyEngine != null) lm.skyEngine.checkBlock(getBlockPos)
        if (block && lm.blockEngine != null) lm.blockEngine.checkBlock(getBlockPos)
    }

//    final def markDescUpdate() {
//        //        val state = world.getBlockState(pos)
//        //        world.notifyBlockUpdate(pos, state, state, 3)
//        val packet = writeStream(0)
//        writeDesc(packet)
//        sendWriteStream(packet)
//    }

    def onScheduledTick():Unit = {}

    final def scheduleTick(time: Int):Unit = {
        val tn = level.getGameTime + time
        if (schedTick > 0L && schedTick < tn) return
        schedTick = tn
        setChanged()
    }

    final def isTickScheduled:Boolean = schedTick >= 0L

    def updateClient():Unit = {}
    def updateServer():Unit = {}

    final override def tick():Unit = {
        if (!level.isClientSide) {
            updateServer()
            val time = level.getGameTime
            if (schedTick >= 0L && time >= schedTick) {
                schedTick = -1L
                onScheduledTick()
                setChanged()
            }
        } else
            updateClient()
    }

    final override def save(tag:CompoundNBT):CompoundNBT = {
        super.save(tag)
        tag.putLong("sched", schedTick)
        saveToNBT(tag)
        tag
    }

    final override def load(state:BlockState, tag:CompoundNBT):Unit = {
        super.load(state, tag)
        schedTick = tag.getLong("sched")
        loadFromNBT(tag)
    }

    /*
     * Following four methods are vanilla's way of handling initial tile
     * data. We are using standard writeDesc function then putting that
     * buffer into a NBT Tag.
     */
    final override def getUpdateTag:CompoundNBT = {
        val tag = super.getUpdateTag
        val out = new MCDataByteBuf
        writeDesc(out)
        out.writeToNBT(tag, "descpkt")
        tag
    }

    final override def handleUpdateTag(state:BlockState, tag:CompoundNBT):Unit = {
        super.handleUpdateTag(state, tag)
        val in = MCDataByteBuf.readFromNBT(tag, "descpkt")
        readDesc(in)
    }

    def saveToNBT(tag:CompoundNBT) {}

    def loadFromNBT(tag:CompoundNBT) {}

    def writeDesc(out:MCDataOutput) {}

    def readDesc(in:MCDataInput) {}

    /**
     * Sends a custom update packet to the other side. If this is serverside,
     * the packet goes to players watching this chunk. From client side, it
     * goes to the server
     * @param key
     * @param writeFunc
     */
    final def sendUpdate(key:Int, writeFunc:MCDataOutput => Unit) {
        val packet = CoreNetwork.createUpdatePacket(this)
        packet.writeByte(key)
        writeFunc(packet)
        if (!level.isClientSide)
            packet.sendToChunk(this)
        else
            packet.sendToServer()
    }

    final def handleRecievedPacket(input:MCDataInput):Unit = {
        val key = input.readUByte()
        readUpdate(key, input)
    }

    def readUpdate(key:Int, input:MCDataInput):Unit = {}
}

trait TTileOrient extends CoreTile {
    var orientation: Byte = 0

    def side:Int = orientation >> 2

    def setSide(s: Int):Unit = {
        val oldOrient = orientation
        orientation = (orientation & 0x3 | s << 2).toByte
        if (oldOrient != orientation) onOrientChanged(oldOrient)
    }

    def rotation:Int = orientation & 0x3

    def setRotation(r: Int):Unit = {
        val oldOrient = orientation
        orientation = (orientation & 0xFC | r).toByte
        if (oldOrient != orientation) onOrientChanged(oldOrient)
    }

    def rotationT:Transformation = Rotation.sideOrientation(side, rotation).at(Vector3.CENTER)

    def onOrientChanged(oldOrient: Int) {}

    // internal r from absRot
    def toInternal(absRot: Int):Int = (absRot + 6 - rotation) % 4

    // absRot from internal r
    def toAbsolute(r: Int):Int = (r + rotation + 2) % 4

    // absDir from absRot
    def absoluteDir(absRot: Int):Int = Rotation.rotateSide(side, absRot)

    // absRot from absDir
    def absoluteRot(absDir: Int):Int = Rotation.rotationTo(side, absDir)
}