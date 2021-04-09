/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.block

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.{Cuboid6, Rotation, Vector3}
import mrtjp.core.world.WorldLib
import net.minecraft.block.{Block, BlockState}
import net.minecraft.entity.Entity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.network.NetworkManager
import net.minecraft.tileentity.TileEntity
import net.minecraft.util._
import net.minecraft.util.math.{BlockPos, BlockRayTraceResult, RayTraceResult}
import net.minecraft.world.{Explosion, IBlockReader, IWorldReader, World}

import java.util.Random
import scala.collection.mutable.ListBuffer

object MultiTileBlock {
}

/*class MultiTileBlock(properties: Block.Properties) extends Block(properties) {

    override def hasTileEntity(state: BlockState) = true

    //    override abstract def createTileEntity(world: World, state: BlockState) = {
    //        var t: MTBlockTile = null
    //        try {
    //            t = tiles(getMetaFromState(state)).newInstance
    //        }
    //        catch {
    //            case e: Exception => e.printStackTrace()
    //        }
    //        t
    //    }

    //    override def isBlockNormalCube(state: BlockState) = false
    //
    //    override def isOpaqueCube(state: BlockState) = false
    //
    //    override def isFullCube(state: BlockState) = false
    //
    //    override def isFullBlock(state: BlockState) = false

    override def ticksRandomly(state: BlockState) = true

    //    override def addCollisionBoxToList(state: BlockState, world: World, pos: BlockPos, entityBox: AxisAlignedBB, collidingBoxes: JList[AxisAlignedBB], entity: Entity, p_185477_7_ : Boolean) {
    //        world.getTileEntity(pos) match {
    //            case t: MTBlockTile =>
    //                val bounds = t.getCollisionBounds.aabb()
    //                val mask = entityBox.offset(-pos.getX, -pos.getY, -pos.getZ)
    //                if (bounds.intersects(mask)) {
    //                    collidingBoxes.add(bounds.offset(pos))
    //                }
    //            case _ =>
    //        }
    //    }

    //    override def getBoundingBox(state: BlockState, world: IBlockAccess, pos: BlockPos) =
    //        world.getTileEntity(pos) match {
    //            case t: MTBlockTile => t.getBlockBounds.aabb()
    //            case _ => super.getBoundingBox(state, world, pos)
    //        }

    //    override def getBlockFaceShape(world: IBlockAccess, state: BlockState, pos: BlockPos, face: EnumFacing) = {
    //        world.getTileEntity(pos) match {
    //            case t: MTBlockTile => t.getBlockFaceShape(face.ordinal)
    //            case _ => BlockFaceShape.UNDEFINED
    //        }
    //    }
    //
    //    @Deprecated //Forge has deprecated this, getBlockFaceShape is the thing to use now.
    //    override final def isSideSolid(state: BlockState, world: IBlockAccess, pos: BlockPos, side: EnumFacing) = {
    //        getBlockFaceShape(world, state, pos, side) == BlockFaceShape.SOLID
    //    }
    //
    //    override def canPlaceTorchOnTop(state: BlockState, world: IBlockAccess, pos: BlockPos) =
    //        world.getTileEntity(pos) match {
    //            case t: MTBlockTile => t.canPlaceTorchOnTop
    //        }

    override def getExplosionResistance(state: BlockState, world: IWorldReader, pos: BlockPos, exploder: Entity, explosion: Explosion) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.getExplosionResistance
            case _ => 0F
        }

    override def getLightValue(state: BlockState, world: IBlockReader, pos: BlockPos) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.getLightValue
            case _ => super.getLightValue(state, world, pos)
        }


    override def getPlayerRelativeBlockHardness(state: BlockState, player: PlayerEntity, world: IBlockReader, pos: BlockPos) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.getHardness
            case _ => super.getPlayerRelativeBlockHardness(state, player, world, pos)
        }

    //    override def removedByPlayer(state: BlockState, world: World, pos: BlockPos, player: PlayerEntity, willHarvest: Boolean) = {
    //        if (world.isRemote) {
    //            true
    //        } else {
    //            val b = state.getBlock
    //            if (b.canHarvestBlock(state, world, pos, player) && !player.abilities.isCreativeMode) {
    //                val stacks = getDrops(world, pos, state, EnchantmentHelper.getEnchantmentLevel(Enchantments.FORTUNE, player.getHeldItemMainhand))
    //                for (stack <- stacks) WorldLib.dropItem(world, pos, stack)
    //            }
    //            world.setBlockToAir(pos)
    //            true
    //        }
    //    }

    override def onReplaced(state: BlockState, world: World, pos: BlockPos, newState: BlockState, isMoving: Boolean) {
        val tile = world.getTileEntity(pos)
        super.onReplaced(state, world, pos, newState, isMoving)
        tile match {
            case t: MTBlockTile => t.onBlockRemoval()
            case _ =>
        }
    }

    //    override def harvestBlock(worldIn: World, player: PlayerEntity, pos: BlockPos, state: BlockState, te: TileEntity, stack: ItemStack) {}

    //    override def getDrops(world: IBlockAccess, pos: BlockPos, state: BlockState, fortune: Int) = {
    //        val list = new ListBuffer[ItemStack]
    //        world.getTileEntity(pos) match {
    //            case t: MTBlockTile => t.addHarvestContents(list)
    //            case _ =>
    //        }
    //        new JArrayList[ItemStack](list)
    //    }

    override def getPickBlock(state: BlockState, target: RayTraceResult, world: IBlockReader, pos: BlockPos, player: PlayerEntity) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.getPickBlock
            case _ => super.getPickBlock(state, target, world, pos, player)
        }


    override def onBlockActivated(state: BlockState, world: World, pos: BlockPos, player: PlayerEntity, handIn: Hand, hit: BlockRayTraceResult) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.onBlockActivated(player, hit.getFace)
            case _ => ActionResultType.PASS
        }

    override def onBlockClicked(world: World, pos: BlockPos, player: PlayerEntity) {
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.onBlockClicked(player)
            case _ =>
        }
    }

    override def onEntityCollidedWithBlock(world: World, pos: BlockPos, state: BlockState, entity: Entity) {
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.onEntityCollision(entity)
            case _ =>
        }
    }

    override def onEntityWalk(world: World, pos: BlockPos, entity: Entity) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.onEntityWalk(entity)
        }

    override def neighborChanged(state: BlockState, world: World, pos: BlockPos, blockIn: Block, fromPos: BlockPos) {
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.onNeighborBlockChange()
            case _ =>
        }
    }

    override def onNeighborChange(world: IBlockAccess, pos: BlockPos, neighbor: BlockPos) {
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.onNeighborTileChange(neighbor)
            case _ =>
        }
    }

    override def onBlockPlacedBy(world: World, pos: BlockPos, state: BlockState, placer: EntityLivingBase, stack: ItemStack) {
        val meta = state.getBlock.getMetaFromState(state)
        if (tiles.isDefinedAt(meta) && tiles(meta) != null) {
            super.onBlockPlacedBy(world, pos, state, placer, stack)
        } else {
            throw new RuntimeException("MultiTileBlock " + this.getRegistryName + " was placed w/ invalid metadata. Most likely an invalid return value on this block's ItemBlock#getMetadata")
        }
    }

    def postBlockSetup(w: World, pos: BlockPos, side: Int, player: EntityPlayer, stack: ItemStack, hit: Vector3) {
        w.getTileEntity(pos) match {
            case t: MTBlockTile => t.onBlockPlaced(side, player, stack)
            case _ =>
        }
    }

    override def getWeakChanges(world: IBlockAccess, pos: BlockPos) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.getWeakChanges
            case _ => false
        }

    override def canProvidePower(state: BlockState) = true

    override def canConnectRedstone(state: BlockState, world: IBlockAccess, pos: BlockPos, side: EnumFacing) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.canConnectRS
            case _ => super.canConnectRedstone(state, world, pos, side)
        }

    override def getStrongPower(state: BlockState, world: IBlockAccess, pos: BlockPos, side: EnumFacing) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.strongPower(side.ordinal)
            case _ => 0
        }

    override def getWeakPower(state: BlockState, world: IBlockAccess, pos: BlockPos, side: EnumFacing) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.weakPower(side.ordinal)
            case _ => 0
        }

    override def isFireSource(world: World, pos: BlockPos, side: EnumFacing) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.isFireSource(side.ordinal)
            case _ => super.isFireSource(world, pos, side)
        }

    override def updateTick(world: World, pos: BlockPos, state: BlockState, rand: Random) =
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.randomTick(rand)
            case _ => super.updateTick(world, pos, state, rand)
        }

    @SideOnly(Side.CLIENT)
    override def getSubBlocks(tab: CreativeTabs, list: NonNullList[ItemStack]) {
        for (i <- tiles.indices) if (tiles(i) != null) {
            list.add(new ItemStack(this, 1, i))
        }
    }

    @SideOnly(Side.CLIENT)
    override def randomDisplayTick(state: BlockState, world: World, pos: BlockPos, rand: Random) {
        world.getTileEntity(pos) match {
            case t: MTBlockTile => t.randomDisplayTick(rand)
            case _ =>
        }
    }
}*/

/*trait TTileOrient extends MTBlockTile {
    var orientation: Byte = 0

    def side = orientation >> 2

    def setSide(s: Int) {
        val oldOrient = orientation
        orientation = (orientation & 0x3 | s << 2).toByte
        if (oldOrient != orientation) onOrientChanged(oldOrient)
    }

    def rotation = orientation & 0x3

    def setRotation(r: Int) {
        val oldOrient = orientation
        orientation = (orientation & 0xFC | r).toByte
        if (oldOrient != orientation) onOrientChanged(oldOrient)
    }

    //def position = new BlockCoord(getPos)

    def rotationT = Rotation.sideOrientation(side, rotation).at(Vector3.center)

    def onOrientChanged(oldOrient: Int) {}

    // internal r from absRot
    def toInternal(absRot: Int) = (absRot + 6 - rotation) % 4

    // absRot from internal r
    def toAbsolute(r: Int) = (r + rotation + 2) % 4

    // absDir from absRot
    def absoluteDir(absRot: Int) = Rotation.rotateSide(side, absRot)

    // absRot from absDir
    def absoluteRot(absDir: Int) = Rotation.rotationTo(side, absDir)
}*/

/*abstract class MTBlockTile extends TileEntity with ICustomPacketTile with ITickable {
    protected var schedTick = -1L

    def onBlockPlaced(side: Int, player: EntityPlayer, stack: ItemStack) {}

    def onBlockRemoval() {}

    def onNeighborBlockChange() {}

    def onNeighborTileChange(neighbor: BlockPos) {}

    def getWeakChanges = false

    def canConnectRS = false

    def strongPower(side: Int) = 0

    def weakPower(side: Int) = strongPower(side)

    def getLightValue = 0

    def isFireSource(side: Int) = false

    def getBlockFaceShape(side: Int) = BlockFaceShape.SOLID

    def canPlaceTorchOnTop = true

    def getExplosionResistance = 0

    def getHardness = 1 / 30F

    def onBlockActivated(player: PlayerEntity, side: Direction) = ActionResultType.PASS

    def onBlockClicked(player: PlayerEntity) = false

    def onEntityCollision(ent: Entity) {}

    def onEntityWalk(ent: Entity) {}

    def getBlockBounds = Cuboid6.full

    def getCollisionBounds = Cuboid6.full

    def onScheduledTick() {}

    def updateClient() {}

    def updateServer() {}

    def randomTick(rand: Random) {}

    @SideOnly(Side.CLIENT)
    def randomDisplayTick(rand: Random) {}

    def getBlock: Block

    def getPickBlock = new ItemStack(getBlock, 1, getBlockMetadata)

    def addHarvestContents(ist: ListBuffer[ItemStack]) {
        ist += getPickBlock
    }

    def x = getPos.getX

    def y = getPos.getY

    def z = getPos.getZ

    def scheduleTick(time: Int) {
        val tn = world.getTotalWorldTime + time
        if (schedTick > 0L && schedTick < tn) return
        schedTick = tn
        markDirty()
    }

    def isTickScheduled = schedTick >= 0L

    def breakBlock_do() {
        val il = new ListBuffer[ItemStack]
        addHarvestContents(il)
        for (stack <- il) WorldLib.dropItem(world, pos, stack)
        world.setBlockToAir(pos)
    }

    override def markDirty() {
        world.markChunkDirty(pos, this)
    }

    final def markRender() {
        world.markBlockRangeForRenderUpdate(pos, pos)
    }

    final def markLight() {
        world.checkLight(pos)
    }

    final def markDescUpdate() {
        //        val state = world.getBlockState(pos)
        //        world.notifyBlockUpdate(pos, state, state, 3)
        val packet = writeStream(0)
        writeDesc(packet)
        sendWriteStream(packet)
    }

    final override def update() {
        if (world.isRemote) {
            updateClient()
            return
        }
        else {
            updateServer()
        }
        if (schedTick < 0L) return
        val time = world.getTotalWorldTime
        if (schedTick <= time) {
            schedTick = -1L
            onScheduledTick()
            markDirty()
        }
    }

    final override def writeToNBT(tag: NBTTagCompound) = {
        super.writeToNBT(tag)
        tag.setLong("sched", schedTick)
        save(tag)
        tag
    }

    final override def readFromNBT(tag: NBTTagCompound) {
        super.readFromNBT(tag)
        schedTick = tag.getLong("sched")
        load(tag)
    }

    /*
     * Following four methods are vanilla's way of handling initial tile
     * data. We are using standard writeDesc function then putting that
     * buffer into a NBT Tag.
     */
    override def getUpdateTag = {
        val tag = super.getUpdateTag
        val out = new PacketCustom(MrTJPCoreSPH.channel, MrTJPCoreSPH.tilePacket) //channel and type dont matter
        writeDesc(out)
        out.toNBTTag(tag)
        tag
    }

    override def handleUpdateTag(tag: NBTTagCompound) {
        super.handleUpdateTag(tag)
        val in = PacketCustom.fromNBTTag(tag)
        readDesc(in)
    }

    override def getUpdatePacket = new SPacketUpdateTileEntity(pos, 0, getUpdateTag)

    override def onDataPacket(net: NetworkManager, pkt: SPacketUpdateTileEntity) {
        handleUpdateTag(pkt.getNbtCompound)
    }

    def save(tag: NBTTagCompound) {}

    def load(tag: NBTTagCompound) {}

    def writeDesc(out: MCDataOutput) {}

    def readDesc(in: MCDataInput) {}

    final override def readFromPacket(packet: MCDataInput) {
        packet.readUByte() match {
            case 0 => readDesc(packet)
            case key => read(packet, key)
        }
    }

    final override def writeToPacket(packet: MCDataOutput) {
        writeDesc(packet.writeByte(0))
    }

    def read(in: MCDataInput, key: Int) {}

    final def writeStream(key: Int): PacketCustom = {
        val stream = new PacketCustom(MrTJPCoreSPH.channel, MrTJPCoreSPH.tilePacket)
        stream.writePos(pos).writeByte(key)
        stream
    }

    final def sendWriteStream(packet: PacketCustom) {
        packet.sendToChunk(world, x >> 4, z >> 4)
    }
}
*/
trait TPacketTile extends TileEntity {

    def writeToPacket(packet: MCDataOutput)

    def readFromPacket(packet: MCDataInput)
}
