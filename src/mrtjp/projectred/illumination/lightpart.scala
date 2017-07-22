package mrtjp.projectred.illumination

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.lighting.LightModel
import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.raytracer.CuboidRayTraceResult
import codechicken.lib.render._
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.pipeline.ColourMultiplier
import codechicken.lib.texture.TextureUtils
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec.uv.IconTransformation
import codechicken.lib.vec.{Rotation, _}
import codechicken.microblock.HollowMicroblock
import codechicken.multipart._
import com.google.common.collect.ImmutableMap
import mrtjp.core.vec.InvertX
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.{PRLib, RenderHalo}
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util._
import net.minecraft.util.math.BlockPos
import net.minecraft.world.{EnumSkyBlock, World}
import net.minecraftforge.common.model.TRSRTransformation
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.opengl.GL11

import scala.collection.JavaConversions._

class BaseLightPart(factory:LightFactory) extends TMultiPart with TCuboidPart with TSlottedPart with TNormalOcclusionPart with IRedstonePart with ILight
{
    protected var powered = false

    var shape:Byte = 0

    def setShape(colour:Int, side:Int, inverted:Boolean)
    {
        //SHAPE: ISSS CCCC
        // C - colour
        // S - side
        // I - inverted
        shape = (colour&0xF).toByte
        shape = (shape|(side&0x7)<<4).toByte
        if (inverted) shape = (shape|0x80).toByte
    }

    def getColor = shape&0xF

    def getSide = (shape>>4)&0x7

    def isInverted = (shape&0x80) != 0

    def preparePlacement(side:Int, meta:Int, inv:Boolean)
    {
        setShape(meta, side, inv)
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setBoolean("pow", powered)
        tag.setByte("sh", shape)
    }

    override def load(tag:NBTTagCompound)
    {
        powered = tag.getBoolean("pow")
        shape = tag.getByte("sh")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(shape).writeBoolean(powered)
    }

    override def readDesc(packet:MCDataInput)
    {
        shape = packet.readByte()
        powered = packet.readBoolean()
    }

    override def read(packet:MCDataInput)
    {
        readDesc(packet)
        updateRender()
    }

    override def onNeighborChanged()
    {
        if (checkSupport) return
        updateState(false)
    }

    def checkSupport:Boolean =
    {
        if (world.isRemote) return false
        if (factory.canFloat) return false
        val bc = pos.offset(EnumFacing.getFront(getSide))

        if (!factory.canFloat && !BaseLightPart.canPlaceLight(world, bc, getSide^1)) {
            TileMultipart.dropItem(getItem, world, Vector3.fromTileCenter(tile))
            tile.remPart(this)
            return true
        }
        false
    }

    override def onPartChanged(part:TMultiPart)
    {
        if (checkSupport) return
        updateState(false)
    }

    override def onAdded()
    {
        if (checkSupport) return
        updateState(true)
    }

    private def checkPower:Boolean =
    {
        for (s <- 0 until 6) if (s != (getSide^1))
            if (RedstoneInteractions.getPowerTo(this, s) > 0)
                return true
        false
    }

    private def updateState(forceRender:Boolean)
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
        if (!world.isRemote) sendDescUpdate()
        tile.recalcLight(false, true)
        tile.markRender()
    }

    override def getLightValue = if (isInverted != powered)
        IlluminationProxy.getLightValue(getColor, 15) else 0

    @SideOnly(Side.CLIENT)
    override def renderDynamic(vec:Vector3, pass:Int, frame:Float)
    {
        if (pass == 0 && isOn)
            RenderHalo.addLight(pos, getColor, getLightBounds)
    }

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState) =
    {
        if (layer == factory.getRenderLayer) {
            ccrs.setBrightness(world, this.pos)
            factory.render(this, getColor, isOn, pos, ccrs)
            true
        }
        else false
    }

    def getItem = new ItemStack(factory.getItem(isInverted), 1, getColor)
    def getLightBounds = factory.getLBounds(getSide)
    override def getBounds = factory.getBounds(getSide)
    override def getType = factory.getType

    override def getStrength(player:EntityPlayer, hit:CuboidRayTraceResult) = 2/30f
    override def getSlotMask = 1<<6
    override def getOcclusionBoxes = Seq(getBounds)

    override def getDrops = Seq(getItem)
    override def pickItem(hit:CuboidRayTraceResult) = getItem

    override def canConnectRedstone(side:Int) = true
    override def strongPowerLevel(side:Int) = 0
    override def weakPowerLevel(side:Int) = 0

    override def isOn = powered != isInverted
}

object BaseLightPart
{
    def canPlaceLight(w:World, pos:BlockPos, side:Int):Boolean =
    {
        if (PRLib.canPlaceLight(w, pos, side)) return true

        val part = BlockMultipart.getPart(w, pos, side)
        if (part.isInstanceOf[HollowMicroblock]) return true

        false
    }
}

class BaseLightFacePart(obj:LightFactory) extends BaseLightPart(obj) with TFacePart with IMaskedRedstonePart
{
    override def solid(side:Int) = false
    override def getSlotMask = (1<<getSide)&0x40

    override def getConnectionMask(s:Int) =
    {
        if ((s^1) == getSide) 0
        else if (s == getSide) 0x10
        else 1<<Rotation.rotationTo(s&6, getSide)
    }
}

trait TAirousLight extends BaseLightPart with ITickable
{
    abstract override def update()
    {
        super.update()
        if (!world.isRemote && isOn) {
            val rad = lightRadius

            val pos1 = pos.add(
                world.rand.nextInt(rad)-world.rand.nextInt(rad),
                Math.max(Math.min(pos.getY+world.rand.nextInt(rad)-world.rand.nextInt(rad), world.getHeight(pos).getY+4), 7),
                world.rand.nextInt(rad)-world.rand.nextInt(rad)
            )

            if (world.isAirBlock(pos1) && world.getLightFor(EnumSkyBlock.BLOCK, pos1) < 8) {
                world.setBlockState(pos, ProjectRedIllumination.blockAirousLight.getDefaultState, 3)
                world.getTileEntity(pos) match {
                    case al:TileAirousLight => al.setSource(pos, getColor, getSide)
                    case _ =>
                }
            }
        }
    }

    def lightRadius = 16
}

abstract class LightFactory extends IPartFactory
{
    private var item:ItemBaseLight = _
    private var itemInv:ItemBaseLight = _

    def getUnlocalizedName(inv:Boolean):String
    def getItemRegistryName(inv:Boolean):String
    def getType:String

    def getBounds(side:Int):Cuboid6
    def getLBounds(side:Int):Cuboid6
    def bakedBoxes(box:Cuboid6) =
    {
        val boxes = new Array[Cuboid6](6)
        boxes(0) = box.copy
        for (s <- 1 until 6)
            boxes(s) = box.copy.apply(Rotation.sideRotations(s).at(Vector3.center))
        boxes
    }

    def getItem(inv:Boolean) = if (inv) itemInv else item

    def createItem(inverted:Boolean):ItemBaseLight = new ItemBaseLight(this, inverted)
    def createPart:BaseLightPart = new BaseLightFacePart(this)

    def canFloat = false

    final def register()
    {
        item = createItem(false)
        item.setUnlocalizedName("projectred.illumination."+getUnlocalizedName(false))
        GameRegistry.register(item.setRegistryName(getItemRegistryName(false)))

        itemInv = createItem(true)
        itemInv.setUnlocalizedName("projectred.illumination."+getUnlocalizedName(true))
        GameRegistry.register(itemInv.setRegistryName(getItemRegistryName(true)))

        MultiPartRegistry.registerParts(this, Array(getType))
    }

    final def makeStack(color:Int, i:Int):ItemStack = new ItemStack(getItem(false), i, color)
    final def makeStack(color:Int):ItemStack = makeStack(color, 1)

    final def makeInvStack(color:Int, i:Int):ItemStack = new ItemStack(getItem(true), i, color)
    final def makeInvStack(color:Int):ItemStack = makeInvStack(color, 1)

    final override def createPart(name:String, client:Boolean) =
        if (name == getType) createPart else null

    @SideOnly(Side.CLIENT)
    final def registerClient()
    {
        val lightState = new CCModelState({
            val builder = ImmutableMap.builder[TransformType, TRSRTransformation]()
            for (tt <- TransformType.values()) {
                val (pos, rot, scale) = getItemRenderTransform(tt)
                val mat = ((new Rotation(rot.z.toRadians, 0, 0, 1) `with`
                    new Rotation(rot.y.toRadians, 0, 1, 0) `with`
                    new Rotation(rot.x.toRadians, 1, 0, 0) `with`
                    new Scale(scale)) at Vector3.center `with`
                    pos.translation()).compile()
                builder.put(tt, TransformUtils.fromMatrix4(mat))
            }
            builder.build()
        })

        val renderer = new IItemRenderer with IIconRegister
        {
            override def isAmbientOcclusion = true
            override def isGui3d = true
            override def getTransforms = lightState

            override def renderItem(item:ItemStack, transformType: TransformType)
            {
                val color = item.getItemDamage%16
                val inv = item.getItem match {
                    case i:ItemBaseLight => i.inverted
                    case _ => false
                }
                renderInv(color, inv, Vector3.zero, CCRenderState.instance())
            }

            override def registerIcons(textureMap:TextureMap)
            {
                registerTextures(textureMap)
            }
        }

        ModelRegistryHelper.registerItemRenderer(getItem(false), renderer)
        ModelRegistryHelper.registerItemRenderer(getItem(true), renderer)

        TextureUtils.addIconRegister(renderer)

        loadModels()
    }

    @SideOnly(Side.CLIENT)
    def loadModels()

    @SideOnly(Side.CLIENT)
    def parseModel(name:String) =
    {
        val models = OBJParser.parseModels(
            new ResourceLocation("projectred", "textures/obj/lighting/"+name+".obj"), 7, InvertX)
        for (m <- models.values()) m.apply(new Translation(0.5, 0, 0.5))
        models
    }

    @SideOnly(Side.CLIENT)
    def bakeCopy(s:Int, m1:CCModel) =
    {
        val m = m1.copy
        m.apply(Rotation.sideOrientation(s, 0).at(Vector3.center))
        finishModel(m)
        m
    }

    @SideOnly(Side.CLIENT)
    def finishModel(m:CCModel) =
    {
        m.computeNormals()
        m.computeLighting(LightModel.standardLightModel)
        m.shrinkUVs(0.0005)
    }

    @SideOnly(Side.CLIENT)
    def getRenderLayer = BlockRenderLayer.SOLID

    def getModelBulb(side:Int):CCModel
    def getModelChassi(side:Int):CCModel

    def getInvModelBulb:CCModel = getModelBulb(0)
    def getInvModelChassi:CCModel = getModelChassi(0)
    def getInvLBounds:Cuboid6 = getLBounds(0)

    @SideOnly(Side.CLIENT)
    def getIcon:TextureAtlasSprite

    @SideOnly(Side.CLIENT)
    def registerTextures(map:TextureMap)

    @SideOnly(Side.CLIENT)
    def render(part:BaseLightPart, color:Int, isOn:Boolean, pos:Vector3, ccrs:CCRenderState)
    {
        val icon = new IconTransformation(getIcon)
        val t = pos.translation()
        getModelChassi(part.getSide).render(ccrs, t, icon)
        getModelBulb(part.getSide).render(ccrs, t, icon, cMult(color, isOn))
    }

    //(pos, rot, scale)
    def getItemRenderTransform(t:TransformType):(Vector3, Vector3, Double) =
    {
        import TransformType._
        t match {
            case GUI => (Vector3.zero, new Vector3(30, 225, 0), 0.625)
            case GROUND => (new Vector3(0, 3/16D, 0), Vector3.zero, 0.25)
            case THIRD_PERSON_RIGHT_HAND => (new Vector3(0, 2.5/16D, 0), new Vector3(75, 45, 0), 0.375)
            case THIRD_PERSON_LEFT_HAND => (new Vector3(0, 2.5/16D, 0), new Vector3(75, 45, 0), 0.375)
            case FIRST_PERSON_RIGHT_HAND => (Vector3.zero, new Vector3(0, 45, 0), 0.4)
            case FIRST_PERSON_LEFT_HAND => (Vector3.zero, new Vector3(0, 225, 0), 0.4)
            case _ => (Vector3.zero, Vector3.zero, 1)
        }
    }

    @SideOnly(Side.CLIENT)
    def renderInv(colour:Int, inverted:Boolean, pos:Vector3, ccrs:CCRenderState)
    {
        val icon = new IconTransformation(getIcon)
        val t = new Translation(pos)

        //Prepair
        TextureUtils.bindBlockTexture()
        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)

        //Render
        getInvModelChassi.render(ccrs, t, icon)
        getInvModelBulb.render(ccrs, t, icon, cMult(colour, inverted))

        //Draw
        ccrs.draw()

        //Draw Halo
        if (inverted) {
            RenderHalo.prepareRenderState()
            RenderHalo.renderHalo(getInvLBounds, colour, t)
            RenderHalo.restoreRenderState()
        }
    }

    def cMult(color:Int, on:Boolean):ColourMultiplier =
    {
        val c = EnumColour.values()(color).getColour
        if (!on) c.multiply(EnumColour.LIGHT_GRAY.getColour)
        ColourMultiplier.instance(c.rgba)
    }
}

