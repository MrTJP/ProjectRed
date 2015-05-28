package mrtjp.projectred.transmission

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.IndexedCuboid6
import codechicken.lib.render.{CCRenderState, TextureUtils}
import codechicken.lib.vec.{BlockCoord, Cuboid6, Rotation, Vector3}
import codechicken.microblock.handler.MicroblockProxy
import codechicken.microblock.{ISidedHollowConnect, ItemMicroPart, MicroMaterialRegistry}
import codechicken.multipart.{PartMap, TMultiPart, TNormalOcclusion, TileMultipart}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.world.PlacementLib
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import mrtjp.projectred.core.libmc.PRLib
import mrtjp.projectred.transmission.IWirePart._
import mrtjp.projectred.transmission.WireDef.WireDef
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition
import org.lwjgl.opengl.GL11

import scala.collection.JavaConversions._

trait TWireCommons extends TMultiPart with TConnectableCommons with TPropagationCommons with TSwitchPacket with TNormalOcclusion
{
    def preparePlacement(side:Int, meta:Int){}

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

    override def diminishOnSide(side:Int) = true

    def debug(player:EntityPlayer) = false

    def test(player:EntityPlayer) = false

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack) =
    {
        //if (CommandDebug.WIRE_READING) debug(player) else
        if (held != null && held.getItem == ProjectRedCore.itemWireDebugger)
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
            TextureUtils.bindAtlas(0)
            CCRenderState.setBrightness(world, x, y, z)
            doStaticTessellation(pos, pass)
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
            CCRenderState.hasColour = false
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

abstract class WirePart extends TMultiPart with TWireCommons with TFaceConnectable with TFacePropagation
{
    override def preparePlacement(side:Int, meta:Int)
    {
        setSide(side^1)
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("connMap", connMap)
        tag.setByte("side", side.toByte)
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
        connMap = packet.readInt()
        orientation = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 1 =>
            connMap = packet.readInt()
            if (useStaticRenderer) tile.markRender()
        case _ => super.read(packet, key)
    }

    override def sendConnUpdate()
    {
        getWriteStreamOf(1).writeInt(connMap)
    }

    override def canConnectCorner(r:Int) = true

    override def canStay =
    {
        val pos = new BlockCoord(tile).offset(side)
        PlacementLib.canPlaceWireOnSide(world, pos.x, pos.y, pos.z, side^1)
    }

    override def getItem = getWireType.makeStack

    override def setRenderFlag(part:IConnectable) = part match
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

abstract class FramedWirePart extends TMultiPart with TWireCommons with TCenterConnectable with TCenterPropagation with ISidedHollowConnect
{
    var hasMaterial = false
    var material = 0

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("connMap", connMap)
        tag.setString("mat", MicroMaterialRegistry.materialName(material))
        tag.setBoolean("hasmat", hasMaterial);tag.setBoolean("nolegacy", true) //TODO Legacy
    }

    override def load(tag:NBTTagCompound)
    {
        connMap = tag.getInteger("connMap")
        hasMaterial = tag.getBoolean("hasmat")
        material = MicroMaterialRegistry.materialID(tag.getString("mat"))
        if (!tag.getBoolean("nolegacy") && material == 0) hasMaterial = false //TODO Legacy
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(clientConnMap)
        packet.writeBoolean(hasMaterial)
        if (hasMaterial)
            MicroMaterialRegistry.writeMaterialID(packet, material)
    }

    override def readDesc(packet:MCDataInput)
    {
        connMap = packet.readUByte()
        hasMaterial = packet.readBoolean()
        if (hasMaterial)
            material = MicroMaterialRegistry.readMaterialID(packet)
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 1 =>
            connMap = packet.readUByte()
            if (useStaticRenderer) tile.markRender()
        case 2 =>
            hasMaterial = true
            material = MicroMaterialRegistry.readMaterialID(packet)
            if (useStaticRenderer) tile.markRender()
        case 3 =>
            hasMaterial = false
            material = 0
            if (useStaticRenderer) tile.markRender()
        case _ =>
    }

    def clientConnMap = connMap&0x3F|connMap>>6&0x3F

    override def sendConnUpdate()
    {
        getWriteStreamOf(1).writeByte(clientConnMap)
    }

    def sendMatUpdate()
    {
        if (hasMaterial) MicroMaterialRegistry.writeMaterialID(getWriteStreamOf(2), material)
        else getWriteStreamOf(3)
    }

    override def discoverOpen(s:Int) = getInternal(s) match
    {
        case null => true
        case w:WirePart if canConnectPart(w, s) => true
        case _ =>
            WireBoxes.expandBounds = s
            val fits = tile.canReplacePart(this, this)
            WireBoxes.expandBounds = -1
            fits
    }

    override def getType = getWireType.framedType

    override def canStay = true

    override def getStrength(hit:MovingObjectPosition, player:EntityPlayer) =
    {
        if (hasMaterial) Math.min(4, MicroMaterialRegistry.getMaterial(material).getStrength(player))
        else 4
    }

    override def getItem = getWireType.makeFramedStack

    override def getDrops =
    {
        if (hasMaterial) super.getDrops :+ ItemMicroPart.create(1, material)
        else super.getDrops
    }

    override def getSubParts = getCollisionBoxes.map(that => new IndexedCuboid6(0, that))

    override def getOcclusionBoxes =
    {
        import mrtjp.projectred.transmission.WireBoxes._
        if (expandBounds >= 0) Seq(fOBounds(expandBounds))
        else Seq(fOBounds(6))
    }

    override def getCollisionBoxes =
    {
        import mrtjp.projectred.transmission.WireBoxes._
        var b = Seq.newBuilder[Cuboid6].+=(fOBounds(6))
        for (s <- 0 until 6) if (maskConnects(s)) b += fOBounds(s)
        b.result()
    }

    override def getHollowSize(side:Int) = 8

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack):Boolean =
    {
        def dropMaterial()
        {
            if (hasMaterial && !player.capabilities.isCreativeMode)
                PRLib.dropTowardsPlayer(world, x, y, z, ItemMicroPart.create(1, material), player)
        }

        if (super.activate(player, hit, held)) return true

        if (held == null && player.isSneaking && hasMaterial)
        {
            if (!world.isRemote)
            {
                dropMaterial()
                hasMaterial = false
                material = 0
                sendMatUpdate()
            }
            return true
        }

        if (held != null && held.getItem == MicroblockProxy.itemMicro && held.getItemDamage == 1)
        {
            val newmatid = ItemMicroPart.getMaterialID(held)
            if (!hasMaterial || newmatid != material)
            {
                if(!world.isRemote)
                {
                    val newmat = MicroMaterialRegistry.getMaterial(newmatid)
                    if (newmat == null || newmat.isTransparent) return false
                    else
                    {
                        dropMaterial()
                        hasMaterial = true
                        material = newmatid
                        world.playSoundEffect(x+0.5D, y+0.5D, z+0.5D, newmat.getSound.func_150496_b(),
                            (newmat.getSound.getVolume+1.0F)/2.0F, newmat.getSound.getPitch*0.8F)
                        sendMatUpdate()
                        if (!player.capabilities.isCreativeMode) held.stackSize-=1
                    }
                }
                return true
            }
        }

        false
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