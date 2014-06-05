package mrtjp.projectred.transmission

import IWirePart._
import codechicken.lib.data.{MCDataOutput, MCDataInput}
import codechicken.lib.raytracer.IndexedCuboid6
import codechicken.lib.render.{TextureUtils, CCRenderState}
import codechicken.lib.vec.{Rotation, Cuboid6, Vector3, BlockCoord}
import codechicken.microblock.handler.MicroblockProxy
import codechicken.microblock.{ISidedHollowConnect, ItemMicroPart, MicroMaterialRegistry}
import codechicken.multipart.{PartMap, TileMultipart, TMultiPart, TNormalOcclusion}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition
import org.lwjgl.opengl.GL11
import scala.collection.JavaConversions._
import mrtjp.projectred.core.libmc.{PRLib, BasicWireUtils}
import net.minecraftforge.common.util.ForgeDirection
import mrtjp.projectred.transmission.WireDef.WireDef

trait TWireCommons extends TMultiPart with TConnectableCommons with TPropagationAcquisitions with TSwitchPacket with TNormalOcclusion
{
    def preparePlacement(side:Int, meta:Int) {}

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isRemote)
        {
            WirePropagator.logCalculation()

            if (updateOutward())
            {
                sendConnUpdate()
                WirePropagator.propagateTo(this, FORCE)
            }
            else WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onNeighborChanged()
    {
        if (!world.isRemote)
        {
            if (dropIfCantStay()) return
            WirePropagator.logCalculation()
            if (updateExternalConns())
            {
                sendConnUpdate()
                WirePropagator.propagateTo(this, FORCE)
            }
            else WirePropagator.propagateTo(this, RISING)
        }
    }

    def propagate(from:TMultiPart, mode:Int)
    def propagateOther(mode:Int) {}

    override def onAdded()
    {
        super.onAdded()
        if (!world.isRemote)
        {
            if (updateInward()) sendConnUpdate()
            WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote) notifyAllExternals()
    }

    override def onChunkLoad()
    {
        if ((connMap&0x80000000) != 0) // converter flag
        {
            if (dropIfCantStay()) return
            connMap = 0
            updateOutward()
            tile.markDirty()
        }
    }

    def sendConnUpdate()

    override def onMaskChanged()
    {
        sendConnUpdate()
    }

    def canStay:Boolean

    def dropIfCantStay() =
    {
        if (!canStay)
        {
            drop()
            true
        }
        else false
    }

    def drop()
    {
        TileMultipart.dropItem(getItem, world, Vector3.fromTileEntityCenter(tile))
        tile.remPart(this)
    }

    def getItem:ItemStack

    def getWireType:WireDef

    def getThickness = getWireType.thickness

    override def getDrops = Seq(getItem)

    override def pickItem(hit:MovingObjectPosition) = getItem

    override def onSignalUpdate()
    {
        tile.markDirty()
    }

    override def isWireSide(side:Int) = true

    def debug(player:EntityPlayer) = false

    def test(player:EntityPlayer) = false

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack) =
    {
        if (CommandDebug.WIRE_READING) debug(player)
        else if (held != null && held.getItem == ProjectRedCore.itemWireDebugger)
        {
            held.damageItem(1, player)
            player.swingItem()
            test(player)
        }
        else false
    }

    def renderHue = -1

    @SideOnly(Side.CLIENT)
    def getIcon = getWireType.wireSprites(0)

    override def doesTick = false

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, pass:Int) =
    {
        if (pass == 0 && useStaticRenderer)
        {
            CCRenderState.setBrightness(world, x, y, z)

            doStaticTessellation(pos, pass)

            CCRenderState.setColour(-1)
            true
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, frame:Float, pass:Int)
    {
        if (pass == 0 && !useStaticRenderer)
        {
            GL11.glDisable(GL11.GL_LIGHTING)
            TextureUtils.bindAtlas(0)
            CCRenderState.hasColour = true
            CCRenderState.startDrawing()

            doDynamicTessellation(pos, frame, pass)

            CCRenderState.draw()
            CCRenderState.setColour(-1)
            GL11.glEnable(GL11.GL_LIGHTING)
        }
    }

    @SideOnly(Side.CLIENT)
    override def drawBreaking(renderBlocks:RenderBlocks)
    {
        CCRenderState.reset()
        doBreakTessellation(renderBlocks)
    }

    @SideOnly(Side.CLIENT)
    def doStaticTessellation(pos:Vector3, pass:Int)
    @SideOnly(Side.CLIENT)
    def doDynamicTessellation(pos:Vector3, frame:Float, pass:Int)
    @SideOnly(Side.CLIENT)
    def doBreakTessellation(renderBlocks:RenderBlocks)

    def useStaticRenderer = Configurator.staticWires
}

abstract class WirePart extends TMultiPart with TWireCommons with TFaceConnectable
{
    override def preparePlacement(side:Int, meta:Int)
    {
        setSide(side^1)
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("connMap", connMap)
        tag.setByte("side", side.asInstanceOf[Byte])
    }

    override def load(tag:NBTTagCompound)
    {
        connMap = tag.getInteger("connMap")
        setSide(tag.getByte("side"))
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeInt(connMap)
        packet.writeByte(orientation)
    }

    override def readDesc(packet:MCDataInput)
    {
        connMap = packet.readInt
        orientation = packet.readByte
    }

    def read(packet:MCDataInput, key:Int) = key match
    {
        case 0 =>
            connMap = packet.readInt
            if (useStaticRenderer) tile.markRender()
    }

    override def sendConnUpdate()
    {
        getWriteStreamOf(0).writeInt(connMap)
    }

    override def canConnectCorner(r:Int) = true

    def canStay =
    {
        val pos = new BlockCoord(tile).offset(side)
        BasicWireUtils.canPlaceWireOnSide(world, pos.x, pos.y, pos.z, ForgeDirection.getOrientation(side^1), false)
    }

    def getItem = getWireType.makeStack

    def setRenderFlag(part:IConnectable) = part match
    {
        case w:WirePart =>
            if (w.getThickness == getThickness) side < w.side else w.getThickness > getThickness
        case _ => true
    }

    override def discoverOpen(r:Int) =
    {
        if (tile.partMap(PartMap.edgeBetween(side, absoluteDir(r))) != null) false
        else getInternal(r) match
        {
            case w:WirePart => canConnectPart(w, r)
            case t:TMultiPart => false
            case null => true
        }
    }

    def propagate(from:TMultiPart, mode:Int)
    {
        if (mode != FORCED) WirePropagator.addPartChange(this)
        for (r <- 0 until 4)
        {
            if (maskConnectsCorner(r)) propagateExternal(getCorner(r), posOfCorner(r), from, mode)
            else if (maskConnectsStraight(r)) propagateExternal(getStraight(r), posOfStraight(r), from, mode)
            else if (maskConnectsInside(r)) propagateInternal(getInternal(r), from, mode)
        }
        if (maskConnectsCenter) propagateInternal(getCenter, from, mode)
        propagateOther(mode)
    }

    override def getType = getWireType.wireType

    override def getStrength(hit:MovingObjectPosition, player:EntityPlayer) = 4

    override def getSubParts = Seq(new IndexedCuboid6(0, WireBoxes.sBounds(getThickness)(side)))

    override def getOcclusionBoxes = Seq(WireBoxes.oBounds(getThickness)(side))

    override def redstoneConductionMap = 0xF

    override def solid(arg0:Int) = false

    @SideOnly(Side.CLIENT)
    override def doBreakTessellation(renderBlocks:RenderBlocks)
    {
        RenderWire.renderBreakingOverlay(renderBlocks.overrideBlockTexture, this)
    }
    @SideOnly(Side.CLIENT)
    override def doDynamicTessellation(pos:Vector3, frame:Float, pass:Int)
    {
        RenderWire.render(this, pos)
    }
    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3, pass:Int)
    {
        RenderWire.render(this, pos)
    }
}

abstract class FramedWirePart extends TMultiPart with TWireCommons with TCenterConnectable with ISidedHollowConnect
{
    var material = 0

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("connMap", connMap)
        tag.setString("mat", MicroMaterialRegistry.materialName(material))
    }

    override def load(tag:NBTTagCompound)
    {
        connMap = tag.getInteger("connMap")
        material = MicroMaterialRegistry.materialID(tag.getString("mat"))
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(clientConnMap)
        MicroMaterialRegistry.writeMaterialID(packet, material)
    }

    override def readDesc(packet:MCDataInput)
    {
        connMap = packet.readUByte()
        material = MicroMaterialRegistry.readMaterialID(packet)
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 0 =>
            connMap = packet.readUByte()
            if (useStaticRenderer) tile.markRender()
        case 1 =>
            material = MicroMaterialRegistry.readMaterialID(packet)
            if (useStaticRenderer) tile.markRender()
        case _ =>
    }

    def clientConnMap = connMap&0x3F|connMap>>6&0x3F

    def sendConnUpdate()
    {
        getWriteStreamOf(0).writeByte(clientConnMap)
    }

    def sendMatUpdate()
    {
        MicroMaterialRegistry.writeMaterialID(getWriteStreamOf(1), material)
    }

    def discoverOpen(s:Int) = getInternal(s) match
    {
        case null => true
        case w:WirePart if canConnectPart(w, s) => true
        case _ =>
            WireBoxes.expandBounds = s
            val fits = tile.canReplacePart(this, this)
            WireBoxes.expandBounds = -1
            fits
    }

    def propagate(from:TMultiPart, mode:Int)
    {
        if (mode != FORCED) WirePropagator.addPartChange(this)
        for (s <- 0 until 6)
            if (maskConnectsOut(s)) propagateExternal(getStraight(s), posOfStraight(s), from, mode)
            else if (maskConnectsIn(s)) propagateInternal(getInternal(s), from, mode)

        propagateOther(mode)
    }

    def getType = getWireType.framedType

    def canStay = true

    override def getStrength(hit:MovingObjectPosition, player:EntityPlayer) =
    {
        if (material > 0) Math.min(4, MicroMaterialRegistry.getMaterial(material).getStrength(player))
        else 4
    }

    def getItem = getWireType.makeFramedStack

    override def getDrops =
    {
        if (material != 0) super.getDrops :+ ItemMicroPart.create(1, material)
        else super.getDrops
    }

    override def getSubParts =
    {
        val b = getCollisionBoxes
        var i = Seq[IndexedCuboid6]()
        for (c <- b) i :+= new IndexedCuboid6(0, c)
        i
    }

    override def getOcclusionBoxes =
    {
        import WireBoxes._
        if (expandBounds >= 0) Seq(fOBounds(expandBounds))
        else Seq(fOBounds(6))
    }

    override def getCollisionBoxes =
    {
        import WireBoxes._
        var boxes = Seq(fOBounds(6))
        for (s <- 0 until 6) if (maskConnects(s)) boxes :+= fOBounds(s)
        boxes
    }

    override def getHollowSize(side:Int) = 8

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack) =
    {
        def dropMaterial()
        {
            if (material > 0 && !player.capabilities.isCreativeMode)
            {
                val drop = ItemMicroPart.create(1, material)
                PRLib.dropTowardsPlayer(world, x, y, z, drop, player)
            }
        }

        if (super.activate(player, hit, held)) true
        else if (held == null)
        {
            if (!world.isRemote && player.isSneaking && material > 0)
            {
                dropMaterial()
                material = 0
                sendMatUpdate()
            }
            false
        }
        else if (held.getItem == MicroblockProxy.itemMicro && held.getItemDamage == 1)
        {
            val newmatid = ItemMicroPart.getMaterialID(held)
            if (newmatid != material)
            {
                if(!world.isRemote)
                {
                    val newmat = MicroMaterialRegistry.getMaterial(newmatid)
                    if (newmat == null || newmat.isTransparent) false
                    else
                    {
                        dropMaterial()
                        material = newmatid
                        world.playSoundEffect(x+0.5D, y+0.5D, z+0.5D, newmat.getSound.func_150496_b(),
                            (newmat.getSound.getVolume+1.0F)/2.0F, newmat.getSound.getPitch*0.8F)
                        sendMatUpdate()
                        if (!player.capabilities.isCreativeMode) held.stackSize-=1
                    }
                }
                true
            }
            else false
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def doBreakTessellation(renderBlocks:RenderBlocks)
    {
        RenderFramedWire.renderBreakingOverlay(renderBlocks.overrideBlockTexture, this)
    }
    @SideOnly(Side.CLIENT)
    override def doDynamicTessellation(pos:Vector3, frame:Float, pass:Int)
    {
        RenderFramedWire.render(this, pos)
    }
    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3, pass:Int)
    {
        RenderFramedWire.render(this, pos)
    }
}

object WireBoxes
{
    var sBounds = Array.ofDim[Cuboid6](3, 6)
    var oBounds = Array.ofDim[Cuboid6](3, 6)

    for (t <- 0 until 3)
    {
        val selection = new Cuboid6(0, 0, 0, 1, (t+2)/16D, 1).expand(-0.005)
        val occlusion = new Cuboid6(2/8D, 0, 2/8D, 6/8D, (t+2)/16D, 6/8D)
        for (s <- 0 until 6)
        {
            sBounds(t)(s) = selection.copy.apply(Rotation.sideRotations(s).at(Vector3.center))
            oBounds(t)(s) = occlusion.copy.apply(Rotation.sideRotations(s).at(Vector3.center))
        }
    }

    var fOBounds =
    {
        val boxes = new Array[Cuboid6](7)
        val w = 2/8D
        boxes(6) = new Cuboid6(0.5-w, 0.5-w, 0.5-w, 0.5+w, 0.5+w, 0.5+w)
        for (s <- 0 until 6)
            boxes(s) = new Cuboid6(0.5-w, 0, 0.5-w, 0.5+w, 0.5-w, 0.5+w).apply(Rotation.sideRotations(s).at(Vector3.center))
        boxes
    }
    var expandBounds = -1
}