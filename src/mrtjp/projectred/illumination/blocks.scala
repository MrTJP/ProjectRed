package mrtjp.projectred.illumination

import java.lang.{Boolean => JBool, Integer => JInt}
import java.util.{Random, List => JList}

import codechicken.lib.block.property.unlisted.{UnlistedBooleanProperty, UnlistedIntegerProperty}
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.model.bakery.{IBakeryProvider, ModelBakery, SimpleBlockRenderer}
import codechicken.lib.model.bakery.generation.IBakery
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.CCRenderState
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec.{Cuboid6, RedundantTransformation}
import codechicken.lib.vec.uv.IconTransformation
import codechicken.multipart.{BlockMultipart, IRedstoneConnectorBlock}
import mrtjp.core.block._
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.RenderHalo
import net.minecraft.block.material.Material
import net.minecraft.block.state.{BlockStateContainer, IBlockState}
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.block.model.BakedQuad
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.EntityLiving.SpawnPlacementType
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util._
import net.minecraft.util.math.BlockPos
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.client.model.IPerspectiveAwareModel
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.apache.commons.lang3.tuple.Triple
import org.lwjgl.opengl.GL11

class BlockLamp extends MultiTileBlock(Material.REDSTONE_LIGHT) with IRedstoneConnectorBlock with IBakeryProvider
{
    setHardness(0.5F)
    setCreativeTab(ProjectRedIllumination.tabLighting)

    override def isBlockNormalCube(state:IBlockState) = true

    override def isOpaqueCube(state:IBlockState) = true

    override def isFullCube(state:IBlockState) = true

    override def isFullBlock(state:IBlockState) = true

    @SideOnly(Side.CLIENT)
    override def getSubBlocks(item:Item, tab:CreativeTabs, list:NonNullList[ItemStack])
    {
        for (i <- 0 until 32)
            list.add(new ItemStack(ProjectRedIllumination.blockLamp, 1, i))
    }

    override def canCreatureSpawn(state:IBlockState, world:IBlockAccess, pos:BlockPos, t:SpawnPlacementType) = false

    override def canConnectRedstone(state:IBlockState, world:IBlockAccess, pos:BlockPos, side:EnumFacing) = true

    override def canProvidePower(state:IBlockState) = false

    override def getConnectionMask(world:IBlockAccess, pos:BlockPos, side:Int) = 0x1F

    override def weakPowerLevel(world:IBlockAccess, pos:BlockPos, side:Int, mask:Int) = 0

    override def createBlockState(): BlockStateContainer = new BlockStateContainer.Builder(this).add(MultiTileBlock.TILE_INDEX)
        .add(BlockProperties.UNLISTED_ON_PROPERTY)
        .add(BlockProperties.UNLISTED_COLOUR_PROPERTY)
        .build()

    override def getExtendedState(state: IBlockState, world: IBlockAccess, pos: BlockPos): IBlockState = ModelBakery.handleExtendedState(state.asInstanceOf[IExtendedBlockState], world, pos)

    @SideOnly(Side.CLIENT)
    override def getBakery: IBakery = LampBakery
}

object BlockProperties {
    val UNLISTED_ON_PROPERTY = new UnlistedBooleanProperty("on")
    val UNLISTED_COLOUR_PROPERTY = new UnlistedIntegerProperty("colour")
}

class ItemBlockLamp extends ItemBlockCore(ProjectRedIllumination.blockLamp)
{
    override def getMetadata(meta:Int) = 0 //we want everything on meta 0, since tiles store the colour
}

object LampBakery extends SimpleBlockRenderer
{
    override def handleState(state: IExtendedBlockState, world:IBlockAccess, pos:BlockPos): IExtendedBlockState = world.getTileEntity(pos) match {
        case t:TileLamp => state.withProperty(BlockProperties.UNLISTED_ON_PROPERTY, t.isOn.asInstanceOf[JBool])
                .withProperty(BlockProperties.UNLISTED_COLOUR_PROPERTY, t.getColor.asInstanceOf[JInt])
        case _ => state
    }


    override def getWorldTransforms(state: IExtendedBlockState) = {
        val isOn = state.getValue(BlockProperties.UNLISTED_ON_PROPERTY)
        val colour = state.getValue(BlockProperties.UNLISTED_COLOUR_PROPERTY)
        val t = new IconTransformation((if (isOn) LampRenderer.iconsOn else LampRenderer.iconsOff)(colour))
        Triple.of(0, 0, t)
    }

    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0,
        new IconTransformation(if (stack.getItemDamage > 15) LampRenderer.iconsOn(stack.getItemDamage%16) else LampRenderer.iconsOff(stack.getItemDamage)))

    override def shouldCull(): Boolean = true

    override def registerIcons(textureMap: TextureMap) {
        for (i <- 0 until 16) {
            LampRenderer.iconsOn(i) = textureMap.registerSprite(new ResourceLocation("projectred:blocks/lighting/lampon/"+i))
            LampRenderer.iconsOff(i) = textureMap.registerSprite(new ResourceLocation("projectred:blocks/lighting/lampoff/"+i))
        }
    }
}

object LampRenderer extends TileEntitySpecialRenderer[TileLamp] with IItemRenderer with IPerspectiveAwareModel
{//TODO, This can be optimized in 1.12. CCL has model wrapping.
    val iconsOn = new Array[TextureAtlasSprite](16)
    val iconsOff = new Array[TextureAtlasSprite](16)

    override def isAmbientOcclusion = true
    override def isGui3d = true
    override def getTransforms = TransformUtils.DEFAULT_BLOCK

    override def renderItem(item:ItemStack, transformType: TransformType)
    {
        import scala.collection.JavaConversions._
        val meta = item.getItemDamage
        val icon = new IconTransformation(if (meta > 15) LampRenderer.iconsOn(meta%16) else LampRenderer.iconsOff(meta))

        //This here is basically a hack as the item model is bound to this IIR.
        val ccrs = CCRenderState.instance()

        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)

        val model = ModelBakery.getCachedItemModel(item)

        renderQuads(model.getQuads(null, null, 0))
        for (face <- EnumFacing.VALUES) {
            renderQuads(model.getQuads(null, face, 0))
        }

        def renderQuads(quads: JList[BakedQuad]) = {
            for (quad:BakedQuad <- quads ) {
                ccrs.getBuffer.addVertexData(quad.getVertexData)
            }
        }

        ccrs.draw()

        if (meta > 15) {
            RenderHalo.prepareRenderState()
            RenderHalo.renderHalo(lBounds, meta%16, new RedundantTransformation)
            RenderHalo.restoreRenderState()
        }
    }

    private val lBounds = Cuboid6.full.copy.expand(0.05D)

    override def renderTileEntityAt(tile:TileLamp, x:Double, y:Double, z:Double, partialTicks:Float, destroyStage:Int)
    {
        if (tile.isOn)
            RenderHalo.addLight(tile.getPos, tile.getColor, lBounds)
    }

}

class TileLamp extends MTBlockTile with ILight
{
    var powered = false
    var shape:Byte = 0

    def setShape(colour:Int, inverted:Boolean)
    {
        shape = (colour&0xF | (if (inverted) 1 else 0) << 4).toByte
    }

    def getColor = shape&0xF
    def getInverted = (shape&0x10) != 0

    override def isOn = getInverted != powered

    override def getBlock = ProjectRedIllumination.blockLamp

    override def getPickBlock = new ItemStack(getBlock, 1, getColor+(if(getInverted) 16 else 0))

    override def onBlockPlaced(side:Int, player:EntityPlayer, stack:ItemStack)
    {
        setShape(stack.getItemDamage%16, stack.getItemDamage > 15)
        //scheduleTick(2)
        updateState(true)
    }

    override def getLightValue = if (getInverted != powered)
        IlluminationProxy.getLightValue(getColor, 15) else 0

    override def onNeighborBlockChange()
    {
        if (!world.isRemote) updateState(false)//scheduleTick(2)
    }

    def checkPower =
    {
        world.isBlockIndirectlyGettingPowered(pos) != 0 ||
            world.getStrongPower(pos) != 0
    }

    def updateState(forceRender:Boolean)
    {
        var updated = false
        if (!world.isRemote) {
            val old = powered
            powered = checkPower
            if (old != powered) {
                updated = true
                updateRender()
            }
        }
        if (forceRender && !updated) updateRender()
    }

    def updateRender()
    {
        if (!world.isRemote) markDescUpdate()
        markLight()
        markRender()
    }

    override def onScheduledTick()
    {
        updateState(false)
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setByte("sh", shape)
        tag.setBoolean("pow", powered)
    }

    override def load(tag:NBTTagCompound)
    {
        shape = tag.getByte("sh")
        powered = tag.getBoolean("pow")
    }

    override def writeDesc(out:MCDataOutput)
    {
        out.writeByte(shape).writeBoolean(powered)
    }

    override def readDesc(in:MCDataInput)
    {
        shape = in.readByte()
        powered = in.readBoolean()
        markRender()
        markLight()
    }
}

class BlockAirousLight extends BlockCore(Material.AIR)
{
    override def getRenderType(state:IBlockState) = EnumBlockRenderType.INVISIBLE

    override def getCollisionBoundingBox(blockState:IBlockState, worldIn:IBlockAccess, pos:BlockPos) = null

    override def isOpaqueCube(state:IBlockState) = false

    override def canCollideCheck(state:IBlockState, hitIfLiquid:Boolean) = false

    override def isReplaceable(worldIn:IBlockAccess, pos:BlockPos) = true

    override def isFullCube(state:IBlockState) = false

    @SideOnly(Side.CLIENT)
    override def randomDisplayTick(state:IBlockState, world:World, pos:BlockPos, rand:Random)
    {
        //TODO get this working
//        if (rand.nextInt(10) > 0) return
//        val color = world.getBlockMetadata(x, y, z)%16
//
//        val dist = 3
//        val dx = x+rand.nextInt(dist)-rand.nextInt(dist)
//        val dy = y+rand.nextInt(dist)-rand.nextInt(dist)
//        val dz = z+rand.nextInt(dist)-rand.nextInt(dist)
//        val ex = dx+rand.nextInt(dist)-rand.nextInt(dist)
//        val ey = dy+rand.nextInt(dist)-rand.nextInt(dist)
//        val ez = dz+rand.nextInt(dist)-rand.nextInt(dist)
//
//        val c = ParticleManagement.instance.spawn(world, "ember", dx, dy, dz)
//        if (c != null)
//        {
//            val orbit = new ParticleLogicOrbitPoint(new Vector3(ex, ey, ez))
//            orbit.setOrbitSpeed(0.5f*rand.nextDouble).setTargetDistance(0.3D)
//            orbit.setShrinkingOrbit(0.01, 0.01).setPriority(2)
//            val scale = new ParticleLogicScale
//            scale.setRate(-0.001F, -0.0001F*rand.nextFloat)
//            scale.setTerminate(true)
//
//            val iconshift = ParticleLogicIconShift.fluttering
//            val approach = new ParticleLogicApproachPoint(new Vector3(ex, ey, ez), 0.03f, 0.5f)
//            approach.setFinal(true)
//
//            c.setIgnoreMaxAge(true)
//            c.setScale(0.05f+0.02f*rand.nextFloat)
//            c.setPRColor(Colors.apply(color))
//            c += orbit
//            c += scale
//            c += iconshift
//            c += approach
//        }
    }

    override def getLightValue(state:IBlockState, world:IBlockAccess, pos:BlockPos) =
        world.getTileEntity(pos) match {
            case t:TileAirousLight => t.lightVal
            case _ => 0
        }
}

class TileAirousLight extends TileEntity with ITickable
{
    private var sourcePos = BlockPos.ORIGIN
    private var sourcePartID = -1
    private var color = -1
    private var delay = 100

    override def update()
    {
        if (!world.isRemote) {
            if ({delay -= 1; delay} > 0) return
            delay = world.rand.nextInt(100)

            val light = getLight
            if (light == null || !light.isOn || light.getColor != color)
                world.setBlockToAir(pos)
        }
    }

    private def getLight:ILight =
    {
        if (sourcePartID > -1) {
            BlockMultipart.getPart(world, sourcePos, sourcePartID) match {
                case light:ILight => return light
                case _ =>
            }
        }
        world.getTileEntity(sourcePos) match {
            case l:ILight => l
            case _ => null
        }
    }

    def setSource(pos:BlockPos, color:Int, partID:Int) =
    {
        sourcePos = pos
        this.color = color
        sourcePartID = partID
    }

    override def readFromNBT(tag:NBTTagCompound)
    {
        super.readFromNBT(tag)
        val x = tag.getInteger("sX")
        val y = tag.getInteger("sY")
        val z = tag.getInteger("sZ")
        sourcePos = new BlockPos(x, y, z)
        sourcePartID = tag.getByte("spID")
        color = tag.getByte("col")
    }

    override def writeToNBT(tag:NBTTagCompound) =
    {
        super.writeToNBT(tag)
        tag.setInteger("sX", sourcePos.getX)
        tag.setInteger("sY", sourcePos.getY)
        tag.setInteger("sX", sourcePos.getZ)
        tag.setByte("spID", sourcePartID.asInstanceOf[Byte])
        tag.setByte("col", color.asInstanceOf[Byte])
        tag
    }

    def lightVal = IlluminationProxy.getLightValue(color, 15)
}
