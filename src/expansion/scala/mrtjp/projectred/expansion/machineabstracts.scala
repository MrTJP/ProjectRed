package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.util.ServerUtils
import codechicken.lib.vec.Rotation
import mrtjp.core.gui.NodeContainer
import mrtjp.core.inventory.{TInventory, TInventoryCapablilityTile}
import mrtjp.projectred.api._
import mrtjp.projectred.core._
import net.minecraft.block.material.Material
import net.minecraft.block.{AbstractBlock, Block, BlockState}
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.player.{PlayerEntity, ServerPlayerEntity}
import net.minecraft.inventory.ISidedInventory
import net.minecraft.inventory.container.{ContainerType, INamedContainerProvider}
import net.minecraft.item.{BlockItemUseContext, ItemStack}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.state.{BooleanProperty, IntegerProperty, StateContainer}
import net.minecraft.tileentity.{TileEntity, TileEntityType}
import net.minecraft.util.math.{BlockPos, BlockRayTraceResult}
import net.minecraft.util.text.{ITextComponent, StringTextComponent}
import net.minecraft.util.{ActionResultType, Hand}
import net.minecraft.world.{IBlockReader, IWorldReader, World}

import scala.jdk.CollectionConverters._
//class BlockMachine(regName:String, bakery:IBlockBakery) extends MultiTileBlock(Material.ROCK) with IBakeryProvider
//{
//    setHardness(2)
//    setCreativeTab(ProjectRedExpansion.tabExpansion)
//
//    override def isOpaqueCube(blockState: IBlockState) = true
//
//    override def isNormalCube(blockState: IBlockState) = true
//
//    override def createBlockState(): BlockStateContainer = new Builder(this).add(MultiTileBlock.TILE_INDEX)
//        .add(BlockProperties.UNLISTED_ROTATION_PROPERTY)
//        .add(BlockProperties.UNLISTED_SIDE_PROPERTY)
//        .add(BlockProperties.UNLISTED_WORKING_PROPERTY)
//        .add(BlockProperties.UNLISTED_CHARGED_PROPERTY)
//        .add(BlockProperties.UNLISTED_BURNING_PROPERTY)
//        .add(BlockProperties.UNLISTED_POWERED_PROPERTY)
//        .add(BlockProperties.UNLISTED_ACTIVE_PROPERTY)
//        .add(BlockProperties.UNLISTED_CHARGE_PROPERTY)
//        .build()
//
//    override def getExtendedState(state: IBlockState, world: IBlockAccess, pos: BlockPos) = ModelBakery.handleExtendedState(state.asInstanceOf[IExtendedBlockState], world, pos)
//
//    override def getBakery:IBakery = bakery
//}
//
//object BlockProperties
//{
//    val UNLISTED_ROTATION_PROPERTY:IUnlistedProperty[Integer] = new UnlistedIntegerProperty("rotation")
//    val UNLISTED_SIDE_PROPERTY = new UnlistedIntegerProperty("side")
//    val UNLISTED_WORKING_PROPERTY = new UnlistedBooleanProperty("working")
//    val UNLISTED_CHARGED_PROPERTY = new UnlistedBooleanProperty("charged")
//    val UNLISTED_BURNING_PROPERTY = new UnlistedBooleanProperty("burning")
//    val UNLISTED_POWERED_PROPERTY = new UnlistedBooleanProperty("powered")
//    val UNLISTED_ACTIVE_PROPERTY = new UnlistedBooleanProperty("active")
//    val UNLISTED_CHARGE_PROPERTY = new UnlistedIntegerProperty("charge")
//}

class BaseMachineBlock(tileFactory:() => CoreTile) extends Block(AbstractBlock.Properties.of(Material.STONE)) {

    override def hasTileEntity(state:BlockState):Boolean = true

    override def createTileEntity(state:BlockState, world:IBlockReader):TileEntity = {
        val tile = tileFactory.apply()
        tile.loadBlockState(state)
        tile
    }

    override def neighborChanged(state:BlockState, world:World, pos:BlockPos, blockIn:Block, fromPos:BlockPos, isMoving:Boolean):Unit = {
        val tile = world.getBlockEntity(pos)
        if (tile != null && tile.isInstanceOf[CoreTile]) {
            tile.asInstanceOf[CoreTile].onNeighborBlockChanged(fromPos)
        }
    }

    override def onNeighborChange(state:BlockState, world:IWorldReader, pos:BlockPos, neighbor:BlockPos):Unit = {
        val tile = world.getBlockEntity(pos)
        if (tile != null && tile.isInstanceOf[CoreTile]) {
            tile.asInstanceOf[CoreTile].onNeighborTileChange(neighbor)
        }
    }

    override def use(state:BlockState, world:World, pos:BlockPos, player:PlayerEntity, handIn:Hand, hit:BlockRayTraceResult):ActionResultType = {
        val tile = world.getBlockEntity(pos)
        if (tile != null && tile.isInstanceOf[CoreTile]) {
            tile.asInstanceOf[CoreTile].onBlockActivated(player, handIn, hit)
        } else
            ActionResultType.FAIL
    }

    override def onRemove(oldState:BlockState, world:World, pos:BlockPos, newState:BlockState, isMoving:Boolean):Unit = {
        val tile = world.getBlockEntity(pos)
        if (tile != null && tile.isInstanceOf[CoreTile]) {
            if (oldState.getBlock == newState.getBlock && newState.hasTileEntity) // state change but same block/tile
                tile.asInstanceOf[CoreTile].onBlockStateReplaced(newState)
            else // complete block change
                tile.asInstanceOf[CoreTile].onBlockRemoved()
        }
        super.onRemove(oldState, world, pos, newState, isMoving) // remove tile if this is a different block now
    }

    override def setPlacedBy(world:World, pos:BlockPos, state:BlockState, player:LivingEntity, stack:ItemStack):Unit = {
        val tile = world.getBlockEntity(pos)
        if (tile != null && tile.isInstanceOf[CoreTile]) {
            tile.asInstanceOf[CoreTile].onBlockPlaced(player, stack)
        }
    }
}

object BaseMachineBlock {
    val SIDE_PROPERTY:IntegerProperty = IntegerProperty.create("side", 0, 5)
    val ROTATION_PROPERTY:IntegerProperty = IntegerProperty.create("rotation", 0, 3)
    val WORKING_PROPERTY:BooleanProperty = BooleanProperty.create("working")
    val CHARGED_PROPERTY:BooleanProperty = BooleanProperty.create("charged")
//    val BURNING_PROPERTY:BooleanProperty = BooleanProperty.create("isBurning")
//    val POWERED_PROPERTY:BooleanProperty = BooleanProperty.create("isPowered")
//    val ACTIVE_PROPERTY:BooleanProperty = BooleanProperty.create("isActive")
    val CHARGE_LEVEL_PROPERTY:IntegerProperty = IntegerProperty.create("charge_level", 0, 8)
}

class RotatableMachineBlock(tileFactory:() => CoreTile) extends BaseMachineBlock(tileFactory) {

    // Ideally this logic would be handled by the tile to keep this block generic,
    // but there isn't a good way to get this `context` to the tile
    override def getStateForPlacement(context: BlockItemUseContext): BlockState = {
        val rot = Rotation.rotationTo(0, context.getHorizontalDirection.ordinal())

        defaultBlockState().setValue(BaseMachineBlock.ROTATION_PROPERTY, Int.box(rot))
    }

    override protected def createBlockStateDefinition(builder: StateContainer.Builder[Block, BlockState]): Unit = {
        builder.add(BaseMachineBlock.ROTATION_PROPERTY)
    }
}

abstract class TileMachine(tileType:TileEntityType[_]) extends CoreTile(tileType)
{
}

trait TOrientableMachine extends TileMachine with TTileOrient {

    override def writeDesc(out: MCDataOutput): Unit = {
        super.writeDesc(out)
        out.writeByte(orientation)
    }

    override def readDesc(in: MCDataInput): Unit = {
        super.readDesc(in)
        orientation = in.readByte
    }

    override def saveToNBT(tag: CompoundNBT) = {
        tag.putByte("rot", orientation)
    }

    override def loadFromNBT(tag: CompoundNBT) = {
        orientation = tag.getByte("rot")
    }

    override def readUpdate(key: Int, in: MCDataInput): Unit = key match {
        case 1 =>
            orientation = in.readByte()
        //            markRender()
        case _ => super.readUpdate(key, in)
    }

    override def onBlockActivated(player: PlayerEntity, hand: Hand, hit: BlockRayTraceResult): ActionResultType = {
        val held = player.getMainHandItem
        if ((doesRotate || doesOrient) && !held.isEmpty && held.getItem.isInstanceOf[IScrewdriver]
            && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held)) {
            def rotate() {
                val old = rotation
                do setRotation((rotation + 1) % 4) while (old != rotation && !isRotationAllowed(rotation))
                if (old != rotation) {
                    sendOrientUpdate()
                    pushState()
                }
                getLevel.blockUpdated(getBlockPos, getBlockState.getBlock)
                onBlockRotated()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }

            def orient() {
                val old = side
                do setSide((side + 1) % 6) while (old != side && !isSideAllowed(side))
                if (old != side) {
                    sendOrientUpdate()
                    pushState()
                }
                getLevel.blockUpdated(getBlockPos, getBlockState.getBlock)
                onBlockRotated()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }

            if (!getLevel.isClientSide) {
                if (player.isCrouching || !doesOrient)
                    rotate()
                else
                    orient()
            }
            ActionResultType.SUCCESS

        } else
            ActionResultType.PASS
    }

    def isRotationAllowed(rot: Int) = true

    def isSideAllowed(s: Int) = true

    def doesRotate = true

    def doesOrient = false

    def sendOrientUpdate(): Unit = {
        sendUpdate(1, _.writeByte(orientation))
    }

    def onBlockRotated() {}
}

trait TGuiMachine extends TileMachine with INamedContainerProvider
{
    abstract override def onBlockActivated(player:PlayerEntity, hand:Hand, hit:BlockRayTraceResult):ActionResultType = {
        val result = super.onBlockActivated(player, hand, hit)

        if (result != ActionResultType.PASS)
            result
        else if (!player.isCrouching) {
            if (!getLevel.isClientSide) openGui(player)
            ActionResultType.SUCCESS
        } else ActionResultType.PASS
    }

    def openGui(player:PlayerEntity):Unit = {
        ServerUtils.openContainer(player.asInstanceOf[ServerPlayerEntity],
            this, (p:MCDataOutput) => p.writePos(getBlockPos))
    }

    override def getDisplayName:ITextComponent = new StringTextComponent(getType.getRegistryName.getPath)
}

trait TPoweredMachine extends TileMachine with TPowerTile with ILowLoadMachine {
    val cond = new PowerConductor(this, idRange) with TPowerDrawPoint

    override def conductor(dir:Int):PowerConductor = cond

    override def canConnectPart(part:IConnectable, s:Int, edgeRot:Int):Boolean = part match {
        case t:ILowLoadPowerLine => true
        case t:ILowLoadMachine => true
        case _ => false
    }

    abstract override def updateServer():Unit = {
        super.updateServer()
        cond.update()
    }

    abstract override def saveToNBT(tag:CompoundNBT) = {
        super.saveToNBT(tag)
        cond.save(tag)
    }

    abstract override def loadFromNBT(tag:CompoundNBT) = {
        super.loadFromNBT(tag)
        cond.load(tag)
    }
}

trait TPoweredOrientableMachine extends TPoweredMachine with TOrientableMachine {
    abstract override def onBlockRotated(): Unit = {
        super.onBlockRotated()
        needsCache = true
    }
}

abstract class TileProcessingMachine(tileType:TileEntityType[_]) extends TileMachine(tileType)
        with TPoweredOrientableMachine
        with TGuiMachine
        with TInventory
        with ISidedInventory
        with TInventoryCapablilityTile
{
    var isCharged = false
    var isWorking = false
    var workRemaining = 0
    var workMax = 0

    override def saveToNBT(tag:CompoundNBT) = {
        super.saveToNBT(tag)
        tag.putBoolean("ch", isCharged)
        tag.putBoolean("work", isWorking)
        tag.putInt("rem", workRemaining)
        tag.putInt("max", workMax)
        saveInv(tag)
    }

    override def loadFromNBT(tag:CompoundNBT) = {
        super.loadFromNBT(tag)
        isCharged = tag.getBoolean("ch")
        isWorking = tag.getBoolean("work")
        workRemaining = tag.getInt("rem")
        workMax = tag.getInt("max")
        loadInv(tag)
    }

    override def writeDesc(out:MCDataOutput):Unit = {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
        out.writeBoolean(isWorking)
    }

    override def readDesc(in:MCDataInput):Unit = {
        super.readDesc(in)
        isCharged = in.readBoolean()
        isWorking = in.readBoolean()
    }

    override def readUpdate(key:Int, in:MCDataInput):Unit = key match {
        case 14 =>
            isCharged = in.readBoolean()
            isWorking = in.readBoolean()
            if (hasLight) {
                markRender()
                recalcLight(false, true)
            }
        case _ => super.readUpdate(key, in)
    }

    def sendWorkUpdate():Unit = {
        sendUpdate(14, _.writeBoolean(isCharged).writeBoolean(isWorking))
    }

    def canStart = false

    def startWork()
    def endWork():Unit = {
        isWorking = false
        workRemaining = 0
        workMax = 0
    }

    def produceResults()

    def calcDoableWork:Int = if (cond.canWork) 1 else 0
    def drainPower(work:Int):Unit = cond.drawPower(work*1100.0D)

    override def updateServer():Unit = {
        super.updateServer()

        if (isWorking) {
            if (workRemaining > 0) {
                val pow = calcDoableWork
                drainPower(pow)
                workRemaining -= pow
            } else {
                endWork()
                produceResults()
            }
        }

        if (!isWorking && calcDoableWork > 0 && canStart) {
            startWork()
        }

        if (getLevel.getGameTime%10 == 0) updateRendersIfNeeded()
    }

    override def setChanged():Unit = {
        super.setChanged()
        if (isWorking && !canStart) endWork()
    }

    override def onBlockRemoved():Unit = {
        super.onBlockRemoved()
        dropInvContents(getLevel, getBlockPos)
    }

    def progressScaled(scale:Int):Int = {
        if (!isWorking || workMax <= 0 || workRemaining <= 0) 0
        else scale*(workMax-workRemaining)/workMax
    }

    private var oldW = isWorking
    private var oldCh = isCharged
    def updateRendersIfNeeded():Unit = {
        isCharged = cond.canWork
        if (isWorking != oldW || isCharged != oldCh) {
            sendWorkUpdate()
            pushState()
        }
        oldW = isWorking
        oldCh = isCharged
    }

    override def loadBlockState(state: BlockState): Unit = {
        super.loadBlockState(state)
        setRotation(state.getValue(BaseMachineBlock.ROTATION_PROPERTY))
    }

    override def covertToBlockState(state: BlockState): BlockState = {
        super.covertToBlockState(state)
            .setValue(BaseMachineBlock.ROTATION_PROPERTY, Int.box(rotation))
            .setValue(BaseMachineBlock.CHARGED_PROPERTY, Boolean.box(isCharged))
            .setValue(BaseMachineBlock.WORKING_PROPERTY, Boolean.box(isWorking))
    }

    def hasLight:Boolean = true
    override def getLightValue:Int = if (isWorking && isCharged) 13 else 0
}

class ContainerPoweredMachine(tile:TPoweredMachine, containerType:ContainerType[_], windowId:Int) extends NodeContainer(containerType, windowId)
{
    private var ch = -2
    private var fl = -2

    override def broadcastChanges():Unit = {
        super.broadcastChanges()
        for (ic <- containerListeners.asScala) {

            if (ch != tile.cond.charge) ic.setContainerData(this, 0, tile.cond.charge)
            if (fl != tile.cond.flow) {
                ic.setContainerData(this, 1, tile.cond.flow&0xFFFF)
                ic.setContainerData(this, 2, tile.cond.flow>>16&0xFFFF)
            }
            ch = tile.cond.charge
            fl = tile.cond.flow
        }
    }

    override def setData(id:Int, bar:Int):Unit = id match {
        case 0 => tile.cond.charge = bar
        case 1 => tile.cond.flow = tile.cond.flow&0xFFFF0000|bar&0xFFFF
        case 2 => tile.cond.flow = tile.cond.flow&0xFFFF|(bar&0xFFFF)<<16
        case _ => super.setData(id, bar)
    }
}

class ContainerProcessingMachine(tile:TileProcessingMachine, containerType:ContainerType[_], windowId:Int) extends ContainerPoweredMachine(tile, containerType, windowId)
{
    private var wr = 0
    private var wm = 0

    override def broadcastChanges():Unit = {
        super.broadcastChanges()
        for (i <- containerListeners.asScala) {
            if (wr != tile.workRemaining) i.setContainerData(this, 3, tile.workRemaining)
            if (wm != tile.workMax) i.setContainerData(this, 4, tile.workMax)
        }
        wr = tile.workRemaining
        wm = tile.workMax
    }

    override def setData(id:Int, bar:Int):Unit = id match {
        case 3 => tile.workRemaining = bar
        case 4 => tile.workMax = bar
        case _ => super.setData(id, bar)
    }
}
