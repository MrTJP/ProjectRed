package mrtjp.projectred.illumination

import codechicken.lib.model.bakedmodels.WrappedItemModel
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec.{Cuboid6, Vector3}
import codechicken.multipart.api.IRedstoneConnectorBlock
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.projectred.core.RenderHalo
import net.minecraft.block.material.Material
import net.minecraft.block.{AbstractBlock, Block, BlockState}
import net.minecraft.client.renderer.IRenderTypeBuffer
import net.minecraft.client.renderer.model.{IBakedModel, IModelTransform, ItemCameraTransforms}
import net.minecraft.client.renderer.tileentity.{TileEntityRenderer, TileEntityRendererDispatcher}
import net.minecraft.item.{BlockItem, BlockItemUseContext, ItemStack}
import net.minecraft.state.StateContainer
import net.minecraft.state.properties.BlockStateProperties
import net.minecraft.tileentity.{TileEntity, TileEntityType}
import net.minecraft.util.Direction
import net.minecraft.util.math.BlockPos
import net.minecraft.world.server.ServerWorld
import net.minecraft.world.{IBlockReader, IWorldReader, World}

import java.util.Random
import java.util.function.Supplier

class IllumarLampBlock(tileSupplier:Supplier[TileEntityType[IllumarLampTile]], val colour:Int, val inverted:Boolean) extends Block(
    AbstractBlock.Properties.of(Material.BUILDABLE_GLASS)
            .strength(0.5F)
            .lightLevel(_ => 15)) with IRedstoneConnectorBlock
{
    registerDefaultState(defaultBlockState.setValue(BlockStateProperties.LIT, Boolean.box(inverted)))

    override def getLightValue(state:BlockState, world:IBlockReader, pos:BlockPos):Int =
        if (state.getValue(BlockStateProperties.LIT)) super.getLightValue(state, world, pos)
        else 0

    override def getStateForPlacement(context:BlockItemUseContext):BlockState =
        this.defaultBlockState().setValue(BlockStateProperties.LIT,
            Boolean.box(context.getLevel.hasNeighborSignal(context.getClickedPos) != inverted))

    override def neighborChanged(state:BlockState, worldIn:World, pos:BlockPos, blockIn:Block, fromPos:BlockPos, isMoving:Boolean) {
        if (!worldIn.isClientSide) {
            val isLit = state.getValue(BlockStateProperties.LIT)
            val shouldBeLit = worldIn.hasNeighborSignal(pos) != inverted

            if (isLit != shouldBeLit)
                if (!worldIn.getBlockTicks.hasScheduledTick(pos, this))
                    worldIn.getBlockTicks.scheduleTick(pos, this, 2)
        }
    }

    override def tick(state:BlockState, worldIn:ServerWorld, pos:BlockPos, rand:Random) {
        val isLit = state.getValue(BlockStateProperties.LIT)
        val shouldBeLit = worldIn.hasNeighborSignal(pos) != inverted

        if (isLit != shouldBeLit)
            worldIn.setBlockAndUpdate(pos, state.setValue(BlockStateProperties.LIT, Boolean.box(shouldBeLit)))
    }

    override protected def createBlockStateDefinition(builder:StateContainer.Builder[Block, BlockState]) {
        builder.add(BlockStateProperties.LIT)
    }

    override def getConnectionMask(world:IWorldReader, pos:BlockPos, side:Int) = 0x1F

    override def weakPowerLevel(world:IWorldReader, pos:BlockPos, side:Int, mask:Int) = 0

    override def canConnectRedstone(state:BlockState, world:IBlockReader, pos:BlockPos, side:Direction):Boolean = true

    override def hasTileEntity(state:BlockState):Boolean = true

    override def createTileEntity(state:BlockState, world:IBlockReader):TileEntity = tileSupplier.get().create()
}

class IllumarLampTile(tileType:TileEntityType[IllumarLampTile], val colour:Int, val inverted:Boolean) extends TileEntity(tileType)
{
    def isOn:Boolean = level.getBlockState(getBlockPos).getValue(BlockStateProperties.LIT)
}

class IllumarLampTileRender(dispatcher:TileEntityRendererDispatcher) extends TileEntityRenderer[IllumarLampTile](dispatcher) {
    override def render(tile:IllumarLampTile, partialTicks:Float, mStack:MatrixStack, buffers:IRenderTypeBuffer, combinedLightIn:Int, combinedOverlayIn:Int):Unit = {
        if (tile.getLevel != null) {
            val state = tile.getLevel.getBlockState(tile.getBlockPos)
            if (state.getBlock.isInstanceOf[IllumarLampBlock] && tile.isOn)
                RenderHalo.renderHalo(CCRenderState.instance(), mStack, buffers, IllumarLampTileRender.glowBounds, tile.colour, Vector3.ZERO)
        }
    }
}

object IllumarLampTileRender {
    val glowBounds = Cuboid6.full.copy.expand(0.05D)
}

class IllumarLampItemRenderer(wrappedModel:IBakedModel) extends WrappedItemModel(wrappedModel) with IItemRenderer {

    override def renderItem(stack:ItemStack, transformType:ItemCameraTransforms.TransformType, mStack:MatrixStack, getter:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int):Unit = {
        renderWrapped(stack, transformType, mStack, getter, packedLight, packedOverlay, false)

        stack.getItem match {
            case il:BlockItem =>
                il.getBlock match {
                    case ib:IllumarLampBlock =>
                        //TODO few open items here:
                        // 1. Iventory block render already takes up most of the space of the slot. Glow needs to be smaller than in-world
                        // 2. Wrapped rendering above does not interact properly with rendering below. Halo seems to be completely
                        //    obscuring block render
                        RenderHalo.renderHalo(CCRenderState.instance(), mStack, getter, Cuboid6.full.copy.expand(0.02D), ib.colour, Vector3.ZERO)
                    case _ =>
                }
            case _ =>
        }
    }

    override def getModelTransform:IModelTransform = TransformUtils.DEFAULT_BLOCK
    override def useAmbientOcclusion:Boolean = true
    override def isGui3d:Boolean = true
    override def usesBlockLight:Boolean = true
}

//class BlockAirousLight extends BlockCore(Material.AIR)
//{
//    override def getRenderType(state:IBlockState) = EnumBlockRenderType.INVISIBLE
//
//    override def getCollisionBoundingBox(blockState:IBlockState, worldIn:IBlockAccess, pos:BlockPos) = null
//
//    override def isOpaqueCube(state:IBlockState) = false
//
//    override def canCollideCheck(state:IBlockState, hitIfLiquid:Boolean) = false
//
//    override def isReplaceable(worldIn:IBlockAccess, pos:BlockPos) = true
//
//    override def isFullCube(state:IBlockState) = false
//
//    @SideOnly(Side.CLIENT)
//    override def randomDisplayTick(state:IBlockState, world:World, pos:BlockPos, rand:Random)
//    {
//        //TODO get this working
////        if (rand.nextInt(10) > 0) return
////        val color = world.getBlockMetadata(x, y, z)%16
////
////        val dist = 3
////        val dx = x+rand.nextInt(dist)-rand.nextInt(dist)
////        val dy = y+rand.nextInt(dist)-rand.nextInt(dist)
////        val dz = z+rand.nextInt(dist)-rand.nextInt(dist)
////        val ex = dx+rand.nextInt(dist)-rand.nextInt(dist)
////        val ey = dy+rand.nextInt(dist)-rand.nextInt(dist)
////        val ez = dz+rand.nextInt(dist)-rand.nextInt(dist)
////
////        val c = ParticleManagement.instance.spawn(world, "ember", dx, dy, dz)
////        if (c != null)
////        {
////            val orbit = new ParticleLogicOrbitPoint(new Vector3(ex, ey, ez))
////            orbit.setOrbitSpeed(0.5f*rand.nextDouble).setTargetDistance(0.3D)
////            orbit.setShrinkingOrbit(0.01, 0.01).setPriority(2)
////            val scale = new ParticleLogicScale
////            scale.setRate(-0.001F, -0.0001F*rand.nextFloat)
////            scale.setTerminate(true)
////
////            val iconshift = ParticleLogicIconShift.fluttering
////            val approach = new ParticleLogicApproachPoint(new Vector3(ex, ey, ez), 0.03f, 0.5f)
////            approach.setFinal(true)
////
////            c.setIgnoreMaxAge(true)
////            c.setScale(0.05f+0.02f*rand.nextFloat)
////            c.setPRColor(Colors.apply(color))
////            c += orbit
////            c += scale
////            c += iconshift
////            c += approach
////        }
//    }
//
//    override def getLightValue(state:IBlockState, world:IBlockAccess, pos:BlockPos) =
//        world.getTileEntity(pos) match {
//            case t:TileAirousLight => t.lightVal
//            case _ => 0
//        }
//}

//class TileAirousLight extends TileEntity with ITickable
//{
//    private var sourcePos = BlockPos.ORIGIN
//    private var sourcePartID = -1
//    private var color = -1
//    private var delay = 100
//
//    override def update()
//    {
//        if (!world.isRemote) {
//            if ({delay -= 1; delay} > 0) return
//            delay = world.rand.nextInt(100)
//
//            val light = getLight
//            if (light == null || !light.isOn || light.getColor != color)
//                world.setBlockToAir(pos)
//        }
//    }
//
//    private def getLight:ILight =
//    {
//        if (sourcePartID > -1) {
//            BlockMultipart.getPart(world, sourcePos, sourcePartID) match {
//                case light:ILight => return light
//                case _ =>
//            }
//        }
//        world.getTileEntity(sourcePos) match {
//            case l:ILight => l
//            case _ => null
//        }
//    }
//
//    def setSource(pos:BlockPos, color:Int, partID:Int) =
//    {
//        sourcePos = pos
//        this.color = color
//        sourcePartID = partID
//    }
//
//    override def readFromNBT(tag:NBTTagCompound)
//    {
//        super.readFromNBT(tag)
//        val x = tag.getInteger("sX")
//        val y = tag.getInteger("sY")
//        val z = tag.getInteger("sZ")
//        sourcePos = new BlockPos(x, y, z)
//        sourcePartID = tag.getByte("spID")
//        color = tag.getByte("col")
//    }
//
//    override def writeToNBT(tag:NBTTagCompound) =
//    {
//        super.writeToNBT(tag)
//        tag.setInteger("sX", sourcePos.getX)
//        tag.setInteger("sY", sourcePos.getY)
//        tag.setInteger("sX", sourcePos.getZ)
//        tag.setByte("spID", sourcePartID.asInstanceOf[Byte])
//        tag.setByte("col", color.asInstanceOf[Byte])
//        tag
//    }
//
//    def lightVal = IlluminationProxy.getLightValue(color, 15)
//}
