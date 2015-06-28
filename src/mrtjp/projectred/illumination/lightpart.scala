package mrtjp.projectred.illumination

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.lighting.LightModel
import codechicken.lib.render.uv.IconTransformation
import codechicken.lib.render.{CCModel, CCRenderState, ColourMultiplier, TextureUtils}
import codechicken.lib.vec._
import codechicken.microblock.HollowMicroblock
import codechicken.multipart._
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors_old
import mrtjp.core.vec.InvertX
import mrtjp.core.world.{PlacementLib, WorldLib}
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.RenderHalo
import mrtjp.projectred.core.libmc.PRLib
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{IIcon, MovingObjectPosition, ResourceLocation}
import net.minecraft.world.{EnumSkyBlock, World}
import net.minecraftforge.client.IItemRenderer.{ItemRenderType, ItemRendererHelper}
import net.minecraftforge.client.{IItemRenderer, MinecraftForgeClient}
import org.lwjgl.opengl.GL11

import scala.collection.JavaConversions._

class BaseLightPart(obj:LightObject) extends TMultiPart with TCuboidPart with TSlottedPart with TNormalOcclusion with IRedstonePart with ILight
{
    protected var inverted = false
    protected var powered = false
    protected var meta:Byte = 0
    var side:Byte = 0

    def preparePlacement(side:Int, meta:Int, inv:Boolean)
    {
        this.inverted = inv
        this.side = side.asInstanceOf[Byte]
        this.meta = meta.asInstanceOf[Byte]
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeBoolean(inverted).writeBoolean(powered)
            .writeByte(meta).writeByte(side)
    }

    override def readDesc(packet:MCDataInput)
    {
        inverted = packet.readBoolean()
        powered = packet.readBoolean()
        meta = packet.readByte()
        side = packet.readByte()
    }

    override def load(tag:NBTTagCompound)
    {
        inverted = tag.getBoolean("inv")
        powered = tag.getBoolean("pow")
        meta = tag.getByte("meta")
        side = tag.getByte("side")
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setBoolean("inv", inverted)
        tag.setBoolean("pow", powered)
        tag.setByte("meta", meta)
        tag.setByte("side", side)
    }

    override def onNeighborChanged()
    {
        if (checkSupport) return
        updateState(false)
    }

    def checkSupport:Boolean =
    {
        if (world.isRemote) return false
        if (obj.canFloat) return false
        val bc = new BlockCoord(tile).offset(side)

        if (!obj.canFloat && !BaseLightPart.canPlaceLight(world, bc.x, bc.y, bc.z, side^1))
        {
            WorldLib.dropItem(world, x, y, z, getItem)
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

    private def checkPower = world.isBlockIndirectlyGettingPowered(x, y, z)

    private def updateState(forceRender:Boolean)
    {
        var updated = false
        if (!world.isRemote)
        {
            val old = powered
            powered = checkPower
            if (old != powered)
            {
                updated = true
                updateRender()
            }
        }
        if (forceRender && !updated) updateRender()
    }

    def updateRender()
    {
        world.markBlockForUpdate(x, y, z)
        world.updateLightByType(EnumSkyBlock.Block, x, y, z)
        if (!world.isRemote) sendDescUpdate()
        tile.markRender()
    }

    override def getLightValue = if (inverted != powered)
        IlluminationProxy.getLightValue(getColor, 15) else 0

    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, frame:Float, pass:Int)
    {
        if (pass == 0 && isOn) RenderHalo.addLight(x, y, z, meta, getLightBounds)
    }

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, pass:Int) =
    {
        if (pass == 0)
        {
            CCRenderState.setBrightness(world, x, y, z)
            obj.render(this, meta, isOn, pos)
            true
        }
        else false
    }

    override def doesTick = false

    def getItem = new ItemStack(obj.getItem(inverted), 1, meta)
    def getLightBounds = obj.getLBounds(side)
    override def getBounds = obj.getBounds(side)
    override def getType = obj.getType

    override def getStrength(hit:MovingObjectPosition, player:EntityPlayer) = 2
    override def getSlotMask = 1<<6
    override def getOcclusionBoxes = Seq(getBounds)

    override def getDrops = Seq(getItem)
    override def pickItem(hit:MovingObjectPosition) = getItem

    override def canConnectRedstone(side:Int) = true
    override def strongPowerLevel(side:Int) = 0
    override def weakPowerLevel(side:Int) = 0

    override def isOn = powered != inverted
    override def getColor = meta
}

object BaseLightPart
{
    def canPlaceLight(w:World, x:Int, y:Int, z:Int, side:Int):Boolean =
    {
        if (PlacementLib.canPlaceLight(w, x, y, z, side)) return true

        val part = PRLib.getMultiPart(w, x, y, z, side)
        if (part.isInstanceOf[HollowMicroblock]) return true

        false
    }
}

class BaseLightFacePart(obj:LightObject) extends BaseLightPart(obj) with TFacePart with IMaskedRedstonePart
{
    override def solid(side:Int) = false
    override def getSlotMask = (1<<side)&0x40

    override def getConnectionMask(s:Int) =
    {
        if ((s^1) == side) 0
        else if (s == side) 0x10
        else 1<<Rotation.rotationTo(s&6, side)
    }
}

trait TAirousLight extends BaseLightPart
{
    abstract override def doesTick = true

    abstract override def update()
    {
        super.update()
        if (!world.isRemote && isOn)
        {
            val rad = lightRadius
            val x1 = x+world.rand.nextInt(rad)-world.rand.nextInt(rad)
            val y1 = Math.max(Math.min(y+world.rand.nextInt(rad)-world.rand.nextInt(rad), world.getHeightValue(x, z)+4), 7)
            val z1 = z+world.rand.nextInt(rad)-world.rand.nextInt(rad)

            if (world.isAirBlock(x1, y1, z1) && world.getBlockLightValue(x1, y1, z1) < 8)
            {
                world.setBlock(x1, y1, z1, ProjectRedIllumination.blockAirousLight, getColor, 3)
                val t = WorldLib.getTileEntity(world, x1, y1, z1, classOf[TileAirousLight])
                if (t != null) t.setSource(x, y, z, getColor, side)
            }
        }
    }

    def lightRadius = 16
}

abstract class LightObject
{
    private var item:ItemBaseLight = null
    private var itemInv:ItemBaseLight = null

    def getItemName:String
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
    final def initServer()
    {
        item = createItem(false)
        itemInv = createItem(true)
    }

    def makeStack(color:Int, i:Int):ItemStack = new ItemStack(item, i, color)
    def makeStack(color:Int):ItemStack = makeStack(color, 1)

    def makeInvStack(color:Int, i:Int):ItemStack = new ItemStack(itemInv, i, color)
    def makeInvStack(color:Int):ItemStack = makeInvStack(color, 1)

    def createItem(inverted:Boolean):ItemBaseLight = new ItemBaseLight(this, inverted)
    def createPart:BaseLightPart = new BaseLightFacePart(this)

    def canFloat = false

    @SideOnly(Side.CLIENT)
    def initClient()
    {
        val renderer = new IItemRenderer
        {
            override def handleRenderType(item:ItemStack, t:ItemRenderType) = true
            override def shouldUseRenderHelper(t:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

            override def renderItem(t:ItemRenderType, item:ItemStack, data:AnyRef*)
            {
                if (0 until 16 contains item.getItemDamage) //else invalid colour
                    renderInv(item.getItemDamage, isInv(item.getItem), t)
            }

            private def isInv(item:Item) = item match
            {
                case i:ItemBaseLight => i.inverted
                case _ => false
            }
        }

        MinecraftForgeClient.registerItemRenderer(item, renderer)
        MinecraftForgeClient.registerItemRenderer(itemInv, renderer)

        loadModels()
    }

    def loadModels()
    def parseModel(name:String) =
    {
        val models = CCModel.parseObjModels(
            new ResourceLocation("projectred", "textures/obj/lighting/"+name+".obj"), 7, InvertX)
        for (m <- models.values()) m.apply(new Translation(0.5, 0, 0.5))
        models
    }
    def bakeCopy(s:Int, m1:CCModel) =
    {
        val m = m1.copy
        m.apply(Rotation.sideOrientation(s, 0).at(Vector3.center))
        finishModel(m)
        m
    }
    def finishModel(m:CCModel) =
    {
        m.computeNormals()
        m.computeLighting(LightModel.standardLightModel)
        m.shrinkUVs(0.0005)
    }

    def getModelBulb(side:Int):CCModel
    def getModelChassi(side:Int):CCModel

    def getInvModelBulb:CCModel = getModelBulb(0)
    def getInvModelChassi:CCModel = getModelChassi(0)
    def getInvLBounds:Cuboid6 = getLBounds(0)

    def getIcon:IIcon

    @SideOnly(Side.CLIENT)
    def registerIcons(reg:IIconRegister)

    @SideOnly(Side.CLIENT)
    def render(part:BaseLightPart, color:Int, isOn:Boolean, pos:Vector3)
    {
        val icon = new IconTransformation(getIcon)
        val t = pos.translation()
        TextureUtils.bindAtlas(0)
        getModelChassi(part.side).render(t, icon)
        getModelBulb(part.side).render(t, icon, cMult(color, isOn))
    }

    import net.minecraftforge.client.IItemRenderer.ItemRenderType._
    def getInvT(t:ItemRenderType):(Vector3, Double) = t match
    {
        case ENTITY => (new Vector3(-0.25D, 0D, -0.25D), 0.75D)
        case EQUIPPED => (new Vector3(-0.15D, 0, -0.15D), 1.5D)
        case EQUIPPED_FIRST_PERSON => (new Vector3(-0.15D, 0, -0.15D), 1.5D)
        case INVENTORY => (new Vector3(0D, -0.05D, 0D), 1D)
        case _ => (Vector3.zero, 1)
    }

    @SideOnly(Side.CLIENT)
    def renderInv(color:Int, inverted:Boolean, t:ItemRenderType)
    {
        val icon = new IconTransformation(getIcon)
        val (pos, scale) = getInvT(t)
        val trans = new Translation(pos)

        //Repair render
        GL11.glPushMatrix()
        GL11.glTranslated(pos.x, pos.y, pos.z)
        GL11.glScaled(scale, scale, scale)
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.setDynamic()
        CCRenderState.pullLightmap()
        CCRenderState.startDrawing()

        //Tessellate
        getInvModelChassi.render(trans, icon)
        getInvModelBulb.render(trans, icon, cMult(color, inverted))

        //Draw
        CCRenderState.draw()

        //Render Halo
        if (inverted)
        {
            RenderHalo.prepareRenderState()
            RenderHalo.renderHalo(getInvLBounds, color, trans)
            RenderHalo.restoreRenderState()
        }

        //Finish
        GL11.glPopMatrix()
    }

    def cMult(color:Int, on:Boolean):ColourMultiplier =
    {
        val c = Colors_old.get(color).c.copy
        if (!on) c.multiply(Colors_old.LIGHT_GREY.c)
        ColourMultiplier.instance(c.rgba)
    }
}

