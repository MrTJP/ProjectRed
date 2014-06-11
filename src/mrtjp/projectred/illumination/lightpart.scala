package mrtjp.projectred.illumination

import codechicken.multipart._
import codechicken.lib.data.{MCDataOutput, MCDataInput}
import net.minecraft.nbt.NBTTagCompound
import codechicken.lib.vec.{Cuboid6, Vector3, BlockCoord}
import mrtjp.projectred.core.libmc.{PRLib, BasicWireUtils}
import net.minecraftforge.common.util.ForgeDirection
import net.minecraft.world.EnumSkyBlock
import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.MovingObjectPosition
import net.minecraft.entity.player.EntityPlayer
import scala.collection.JavaConversions._
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraftforge.client.{MinecraftForgeClient, IItemRenderer}
import net.minecraftforge.client.IItemRenderer.{ItemRendererHelper, ItemRenderType}
import mrtjp.projectred.ProjectRedIllumination

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
        val bc = new BlockCoord(x, y, z).offset(side)
        if (!BasicWireUtils.canPlaceWireOnSide(world, bc.x, bc.y, bc.z, ForgeDirection.getOrientation(side^1), false)
            && !(BasicWireUtils.canPlaceTorchOnBlock(world, bc.x, bc.y, bc.z, false) && (side^1) == 0))
        {
            PRLib.dropItem(world, x, y, z, getItem)
            tile.remPart(this)
            return true
        }
        false
    }

    override def onPartChanged(part:TMultiPart){updateState(false)}

    override def onAdded(){updateState(true)}

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

    override def getLightValue = if (inverted != powered) 15 else 0

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

class BaseLightFacePart(obj:LightObject) extends BaseLightPart(obj) with TFacePart
{
    override def solid(side:Int) = false
    override def getSlotMask = 1<<side
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
                val t = PRLib.getTileEntity(world, x1, y1, z1, classOf[TileAirousLight])
                if (t != null) t.setSource(x, y, z, getColor, side)
            }
        }
    }

    def lightRadius = 16
}

abstract class LightObject
{
    def getItemName:String
    def getType:String

    def getBounds(side:Int):Cuboid6
    def getLBounds(side:Int):Cuboid6

    private var item:ItemBaseLight = null
    private var itemInv:ItemBaseLight = null

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
    }

    @SideOnly(Side.CLIENT)
    def registerIcons(reg:IIconRegister){}

    @SideOnly(Side.CLIENT)
    def render(part:BaseLightPart, color:Int, isOn:Boolean, pos:Vector3)

    @SideOnly(Side.CLIENT)
    def renderInv(color:Int, inverted:Boolean, t:ItemRenderType)
}

