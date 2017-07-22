package mrtjp.projectred.transmission

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.{CuboidRayTraceResult, IndexedCuboid6}
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.{Cuboid6, Rotation, Vector3}
import codechicken.microblock.handler.MicroblockProxy
import codechicken.microblock.{ISidedHollowConnect, ItemMicroPart, MicroMaterialRegistry}
import codechicken.multipart._
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import mrtjp.projectred.transmission.IWirePart._
import mrtjp.projectred.transmission.WireDef.WireDef
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{BlockRenderLayer, EnumFacing, EnumHand, SoundCategory}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.JavaConversions._

trait TWireCommons extends TMultiPart with TConnectableCommons with TPropagationCommons with TSwitchPacket with TNormalOcclusionPart
{
    def preparePlacement(side:Int, meta:Int){}

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isRemote) {
            WirePropagator.logCalculation()

            if (updateOutward()) {
                onMaskChanged()
                WirePropagator.propagateTo(this, FORCE)
            }
            else WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onNeighborChanged()
    {
        if (!world.isRemote) {
            if (dropIfCantStay()) return
            WirePropagator.logCalculation()
            if (updateExternalConns()) {
                onMaskChanged()
                WirePropagator.propagateTo(this, FORCE)
            }
            else WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onAdded()
    {
        super.onAdded()
        if (!world.isRemote) {
            if (updateInward()) onMaskChanged()
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
        if (!canStay) {
            drop()
            true
        }
        else false
    }

    def drop()
    {
        TileMultipart.dropItem(getItem, world, Vector3.fromTileCenter(tile))
        tile.remPart(this)
    }

    def getItem:ItemStack

    def getWireType:WireDef

    def getThickness = getWireType.thickness

    override def getDrops = Seq(getItem)

    override def pickItem(hit:CuboidRayTraceResult) = getItem

    override def onSignalUpdate()
    {
        tile.markDirty()
    }

    override def diminishOnSide(side:Int) = true

    def debug(player:EntityPlayer) = false

    def test(player:EntityPlayer) = false

    override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, held:ItemStack, hand:EnumHand) =
    {
        //if (CommandDebug.WIRE_READING) debug(player) else
        if (held != null && held.getItem == ProjectRedCore.itemMultimeter) {
            held.damageItem(1, player)
            player.swingArm(hand)
            test(player)
        }
        else false
    }

    def renderHue = -1

    @SideOnly(Side.CLIENT)
    def getIcon = getWireType.wireSprites(0)

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState) =
    {
        if (layer == getRenderLayer && useStaticRenderer) {
            ccrs.setBrightness(world, this.pos)
            doStaticTessellation(pos, layer, ccrs)
            true
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def renderFast(pos:Vector3, pass:Int, frame:Float, ccrs:CCRenderState)
    {
        if (pass == 0 && !useStaticRenderer) {
            //GL11.glDisable(GL11.GL_LIGHTING)
            TextureUtils.bindBlockTexture()
            //ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.BLOCK)

            doDynamicTessellation(pos, frame, pass, ccrs)

            //ccrs.draw()
            //GL11.glEnable(GL11.GL_LIGHTING)
        }
    }

    @SideOnly(Side.CLIENT)
    override def renderBreaking(pos:Vector3, texture:TextureAtlasSprite, ccrs:CCRenderState)
    {
        ccrs.reset()
        doBreakTessellation(pos, texture, ccrs)
    }

    @SideOnly(Side.CLIENT)
    def getRenderLayer = BlockRenderLayer.SOLID

    override def canRenderFast = true

    @SideOnly(Side.CLIENT)
    def doStaticTessellation(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState)
    @SideOnly(Side.CLIENT)
    def doDynamicTessellation(pos:Vector3, frame:Float, pass:Int, ccrs:CCRenderState)
    @SideOnly(Side.CLIENT)
    def doBreakTessellation(pos:Vector3, texture:TextureAtlasSprite, ccrs:CCRenderState)

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

    override def canStay = PRLib.canPlaceWireOnSide(world,
        pos.offset(EnumFacing.getFront(side)), side^1)

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
        else getInternal(r) match {
            case w:WirePart => canConnectPart(w, r)
            case t:TMultiPart => false
            case null => true
        }
    }

    override def getType = getWireType.wireType

    override def getStrength(player:EntityPlayer, hit:CuboidRayTraceResult) = 2/30f

    override def getSubParts = Seq(new IndexedCuboid6(0, WireBoxes.sBounds(getThickness)(side)))

    override def getOcclusionBoxes = Seq(WireBoxes.oBounds(getThickness)(side))

    override def redstoneConductionMap = 0xF

    override def solid(side:Int) = false

    @SideOnly(Side.CLIENT)
    override def doBreakTessellation(pos:Vector3, texture:TextureAtlasSprite, ccrs:CCRenderState)
    {
        RenderWire.renderBreakingOverlay(texture, this, ccrs)
    }
    @SideOnly(Side.CLIENT)
    override def doDynamicTessellation(pos:Vector3, frame:Float, pass:Int, ccrs:CCRenderState)
    {
        RenderWire.render(this, pos, ccrs)
    }
    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState)
    {
        RenderWire.render(this, pos, ccrs)
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
        tag.setBoolean("hasmat", hasMaterial)
    }

    override def load(tag:NBTTagCompound)
    {
        connMap = tag.getInteger("connMap")
        hasMaterial = tag.getBoolean("hasmat")
        material = MicroMaterialRegistry.materialID(tag.getString("mat"))
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

    override def getStrength(player:EntityPlayer, hit:CuboidRayTraceResult) =
    {
        if (hasMaterial) Math.min(1.25f/30f, MicroMaterialRegistry.getMaterial(material).getStrength(player))
        else 1.25f/30f
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

    override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, held:ItemStack, hand:EnumHand):Boolean =
    {
        def dropMaterial()
        {
            if (hasMaterial && !player.capabilities.isCreativeMode)
                PRLib.dropTowardsPlayer(world, pos, ItemMicroPart.create(1, material), player)
        }

        if (super.activate(player, hit, held, hand)) return true

        if (held == null && player.isSneaking && hasMaterial) {
            if (!world.isRemote) {
                dropMaterial()
                hasMaterial = false
                material = 0
                sendMatUpdate()
            }
            return true
        }

        if (held != null && held.getItem == MicroblockProxy.itemMicro && held.getItemDamage == 1) {
            val newmatid = ItemMicroPart.getMaterialID(held)
            if (!hasMaterial || newmatid != material) {
                if(!world.isRemote) {
                    val newmat = MicroMaterialRegistry.getMaterial(newmatid)
                    if (newmat == null || newmat.isTransparent) return false
                    else {
                        dropMaterial()
                        hasMaterial = true
                        material = newmatid
                        world.playSound(null, pos, newmat.getSound.getPlaceSound,
                            SoundCategory.BLOCKS, newmat.getSound.getVolume+1.0F/2.0F,
                            newmat.getSound.getPitch*0.8F)
                        sendMatUpdate()
                        if (!player.capabilities.isCreativeMode) held.shrink(1)
                    }
                }
                return true
            }
        }

        false
    }

    @SideOnly(Side.CLIENT)
    override def getRenderLayer = BlockRenderLayer.CUTOUT

    @SideOnly(Side.CLIENT)
    override def doBreakTessellation(pos:Vector3, texture:TextureAtlasSprite, ccrs:CCRenderState)
    {
        RenderFramedWire.renderBreakingOverlay(texture, this, ccrs)
    }
    @SideOnly(Side.CLIENT)
    override def doDynamicTessellation(pos:Vector3, frame:Float, pass:Int, ccrs:CCRenderState)
    {
        RenderFramedWire.render(this, pos, ccrs)
    }
    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState)
    {
        RenderFramedWire.render(this, pos, ccrs)
    }
}

object WireBoxes
{
    var sBounds = Array.ofDim[Cuboid6](3, 6)
    var oBounds = Array.ofDim[Cuboid6](3, 6)

    for (t <- 0 until 3) {
        val selection = new Cuboid6(0, 0, 0, 1, (t+2)/16D, 1).expand(-0.005)
        val occlusion = new Cuboid6(2/8D, 0, 2/8D, 6/8D, (t+2)/16D, 6/8D)
        for (s <- 0 until 6) {
            sBounds(t)(s) = selection.copy.apply(Rotation.sideRotations(s).at(Vector3.center))
            oBounds(t)(s) = occlusion.copy.apply(Rotation.sideRotations(s).at(Vector3.center))
        }
    }

    var fOBounds = {
        val boxes = new Array[Cuboid6](7)
        val w = 2/8D
        boxes(6) = new Cuboid6(0.5-w, 0.5-w, 0.5-w, 0.5+w, 0.5+w, 0.5+w)
        for (s <- 0 until 6)
            boxes(s) = new Cuboid6(0.5-w, 0, 0.5-w, 0.5+w, 0.5-w, 0.5+w).apply(Rotation.sideRotations(s).at(Vector3.center))
        boxes
    }
    var expandBounds = -1
}
