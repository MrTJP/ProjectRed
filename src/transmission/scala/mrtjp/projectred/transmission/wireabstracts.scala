package mrtjp.projectred.transmission

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.{IndexedVoxelShape, VoxelShapeCache}
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.buffer.TransformingVertexBuilder
import codechicken.lib.vec.{Cuboid6, Rotation, Vector3}
import codechicken.microblock.api.{ISidedHollowConnect, MicroMaterial}
import codechicken.microblock.handler.MicroblockModContent
import codechicken.microblock.{ItemMicroBlock, MicroMaterialRegistry}
import codechicken.multipart.api.part.{TMultiPart, TNormalOcclusionPart}
import codechicken.multipart.block.TileMultiPart
import codechicken.multipart.util.{PartMap, PartRayTraceResult}
import com.google.common.collect.ImmutableSet
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.IWirePart._
import mrtjp.projectred.core._
import net.minecraft.block.SoundType
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType}
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.inventory.EquipmentSlotType
import net.minecraft.item.{ItemStack, ItemUseContext}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.shapes.{VoxelShape, VoxelShapes}
import net.minecraft.util.{ActionResultType, Direction, Hand, SoundCategory}
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

import java.util.{Collections, Collection => JCollection}
import scala.jdk.CollectionConverters._

trait TWireCommons extends TMultiPart with TConnectableCommons with TPropagationCommons with TSwitchPacket with TNormalOcclusionPart
{
    def preparePlacement(side:Direction){}

    override def getPlacementSound(context: ItemUseContext) = SoundType.GLASS

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isClientSide) {
            WirePropagator.logCalculation()

            if (updateOutward()) {
                onMaskChanged()
                WirePropagator.propagateTo(this, FORCE)
            }
            else WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onNeighborBlockChanged(from: BlockPos)
    {
        if (!world.isClientSide) {
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
        if (!world.isClientSide) {
            if (updateInward()) onMaskChanged()
            WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isClientSide) notifyAllExternals()
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
        TileMultiPart.dropItem(getItem, world, Vector3.fromTileCenter(tile))
        tile.remPart(this)
    }

    def getItem:ItemStack

    def getWireType:WireType

    def getThickness = getWireType.getThickness

    override def getDrops:JCollection[ItemStack] = Collections.singleton(getItem)

    override def pickItem(hit:PartRayTraceResult) = getItem

    override def onSignalUpdate()
    {
        tile.setChanged()
    }

    override def diminishOnSide(side:Int) = true

    def debug(player:PlayerEntity) = false

    def test(player:PlayerEntity) = false

    override def activate(player:PlayerEntity, hit:PartRayTraceResult, held:ItemStack, hand:Hand): ActionResultType =
    {
        //if (CommandDebug.WIRE_READING) debug(player) else
        if (!held.isEmpty && held.getItem == CoreContent.itemMultimeter.get()) {
            held.hurtAndBreak(1, player, (p:PlayerEntity) => p.broadcastBreakEvent(EquipmentSlotType.MAINHAND))
            player.swing(hand)
            if(test(player))
                return ActionResultType.SUCCESS
        }
        ActionResultType.PASS
    }

    def renderHue = -1

    @OnlyIn(Dist.CLIENT)
    def getIcon = getWireType.getTextures.get(0)

    @OnlyIn(Dist.CLIENT)
    override def renderStatic(layer:RenderType, ccrs:CCRenderState) =
    {
        if (layer == null || (layer == getRenderLayer && useStaticRenderer)) {
            ccrs.setBrightness(world, this.pos)
            doStaticTessellation(layer, ccrs)
            true
        }
        else false
    }

    @OnlyIn(Dist.CLIENT)
    override def renderDynamic(mStack: MatrixStack, buffers: IRenderTypeBuffer, packedLight: Int, packedOverlay: Int, partialTicks: Float)
    {
        if(!useStaticRenderer) doFastTessellation(mStack, buffers, packedLight, packedOverlay, partialTicks)
    }

    @OnlyIn(Dist.CLIENT)
    def getRenderLayer = RenderType.solid()

    @OnlyIn(Dist.CLIENT)
    def doStaticTessellation(layer:RenderType, ccrs:CCRenderState)

    @OnlyIn(Dist.CLIENT)
    def doFastTessellation(mStack: MatrixStack, buffers: IRenderTypeBuffer, packedLight: Int, packedOverlay: Int, partialTicks: Float)

    def useStaticRenderer = Configurator.staticWires
}

abstract class WirePart(wireType:WireType) extends TMultiPart with TWireCommons with TFaceConnectable with TFacePropagation
{
    override final def getWireType = wireType

    override final def getType = getWireType.getPartType

    override def preparePlacement(side: Direction)
    {
        setSide(side.ordinal() ^ 1)
    }

    override def save(tag:CompoundNBT)
    {
        tag.putInt("connMap", connMap)
        tag.putByte("side", side.toByte)
    }

    override def load(tag:CompoundNBT)
    {
        connMap = tag.getInt("connMap")
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
        sendUpdate(1, _.writeInt(connMap))
    }

    override def canConnectCorner(r:Int) = true

    override def canStay = PRLib.canPlaceWireOnSide(world,
        pos.relative(Direction.values()(side)), Direction.values()(side^1))

    override def getItem = getWireType.makeStack

    override def setRenderFlag(part:IConnectable) = part match
    {
        case w:WirePart =>
            if (w.getThickness == getThickness) side < w.side else w.getThickness > getThickness
        case _ => true
    }

    override def discoverOpen(r:Int) =
    {
        if (tile.getSlottedPart(PartMap.edgeBetween(side, absoluteDir(r))) != null) false
        else getInternal(r) match {
            case w:WirePart => canConnectPart(w, r)
            case t:TMultiPart => false
            case null => true
        }
    }

    override def getStrength(player:PlayerEntity, hit:PartRayTraceResult) = 2/30f

    override def getOutlineShape = new IndexedVoxelShape(WireBoxes.sShapes(getThickness)(side), 0)

    override def getCollisionShape = VoxelShapes.empty()

    override def getOcclusionShape = WireBoxes.oShapes(getThickness)(side)

    override def redstoneConductionMap = 0xF

    override def solid(side:Int) = false

    @OnlyIn(Dist.CLIENT)
    override def doFastTessellation(mStack: MatrixStack, buffers: IRenderTypeBuffer, packedLight: Int, packedOverlay: Int, partialTicks: Float)
    {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.brightness = packedLight
        ccrs.overlay = packedOverlay
        ccrs.bind(new TransformingVertexBuilder(buffers.getBuffer(RenderType.solid()), mStack), DefaultVertexFormats.BLOCK)
        RenderWire.render(this, ccrs)
    }
    @OnlyIn(Dist.CLIENT)
    override def doStaticTessellation(layer:RenderType, ccrs:CCRenderState)
    {
        RenderWire.render(this, ccrs)
    }
}

abstract class FramedWirePart(wireType:WireType) extends TMultiPart with TWireCommons with TCenterConnectable with TCenterPropagation with ISidedHollowConnect
{
    var material:MicroMaterial = null

    override final def getWireType = wireType

    override final def getType = getWireType.getPartType

    override def save(tag:CompoundNBT)
    {
        tag.putInt("connMap", connMap)
        if (material != null) {
            tag.putString("mat", material.getRegistryName.toString)
        }
    }

    override def load(tag:CompoundNBT)
    {
        connMap = tag.getInt("connMap")
        if (tag.contains("mat")) {
            material = MicroMaterialRegistry.getMaterial(tag.getString("mat"))
        }
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(clientConnMap)
        packet.writeBoolean(material != null)
        if (material != null) {
            packet.writeRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS, material)
        }
    }

    override def readDesc(packet:MCDataInput)
    {
        connMap = packet.readUByte()
        if (packet.readBoolean()) {
            material = packet.readRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS)
        }
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 1 =>
            connMap = packet.readUByte()
            if (useStaticRenderer) tile.markRender()
        case 2 =>
            material = packet.readRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS)
            if (useStaticRenderer) tile.markRender()
        case 3 =>
            material = null
            if (useStaticRenderer) tile.markRender()
        case _ =>
    }

    def clientConnMap = connMap&0x3F|connMap>>6&0x3F

    override def sendConnUpdate()
    {
        sendUpdate(1, _.writeByte(clientConnMap))
    }

    def sendMatUpdate()
    {
        if (material != null) {
            sendUpdate(2, _.writeRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS, material))
        } else sendUpdate(3)
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

    override def canStay = true

    override def getStrength(player:PlayerEntity, hit:PartRayTraceResult) =
    {
        if (material != null) Math.min(1.25f/30f, material.getStrength(player))
        else 1.25f/30f
    }

    override def getItem = getWireType.makeStack

    override def getDrops =
    {
        if (material != null) (super.getDrops.asScala.toSeq :+ ItemMicroBlock.create(0, 1, material)).asJava
        else super.getDrops
    }

    override def getOutlineShape = new IndexedVoxelShape(getCollisionShape, 0)

    override def getOcclusionShape =
    {
        import mrtjp.projectred.transmission.WireBoxes._
        if (expandBounds >= 0) fOShapes(expandBounds)
        else fOShapes(6)
    }

    override def getCollisionShape =
    {
        import mrtjp.projectred.transmission.WireBoxes._
        var m = 0
        for (s <- 0 until 6) if (maskConnects(s)) m |= 1 << s
        fOShapeStates(m)
    }

    override def getHollowSize(side:Int) = 8

    override def activate(player:PlayerEntity, hit:PartRayTraceResult, held:ItemStack, hand:Hand):ActionResultType = {
        def dropMaterial():Unit = {
            if (material != null && !player.abilities.instabuild)
                PRLib.dropTowardsPlayer(world, pos, ItemMicroBlock.create(0, 1, material), player)
        }

        if (super.activate(player, hit, held, hand).shouldSwing) return ActionResultType.SUCCESS

        if (held.isEmpty && player.isCrouching && material != null) {
            if (!world.isClientSide) {
                dropMaterial()
                material = null
                sendMatUpdate()
            }
            return ActionResultType.SUCCESS
        }

        if (!held.isEmpty && held.getItem == MicroblockModContent.itemMicroBlock && MicroMaterialRegistry.microFactory(held) == 0 && MicroMaterialRegistry.microSize(held) == 1) {
            val newMat = ItemMicroBlock.getMaterialFromStack(held)
            if (material == null || newMat != material) {
                if(!world.isClientSide) {
                    if (newMat == null || newMat.isTransparent) return ActionResultType.PASS
                    else {
                        dropMaterial()
                        material = newMat
                        world.playSound(null, pos, newMat.getSound.getPlaceSound,
                            SoundCategory.BLOCKS, newMat.getSound.getVolume+1.0F/2.0F,
                            newMat.getSound.getPitch*0.8F)
                        sendMatUpdate()
                        if (!player.abilities.instabuild) held.shrink(1)
                    }
                }
                return ActionResultType.SUCCESS
            }
        }

        ActionResultType.PASS
    }

    @OnlyIn(Dist.CLIENT)
    override def getRenderLayer = RenderType.cutout()

    @OnlyIn(Dist.CLIENT)
    override def doFastTessellation(mStack: MatrixStack, buffers: IRenderTypeBuffer, packedLight: Int, packedOverlay: Int, partialTicks: Float)
    {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.brightness = packedLight
        ccrs.overlay = packedOverlay
        ccrs.bind(new TransformingVertexBuilder(buffers.getBuffer(RenderType.solid()), mStack), DefaultVertexFormats.BLOCK)
        RenderFramedWire.render(this, ccrs)
    }

    @OnlyIn(Dist.CLIENT)
    override def doStaticTessellation(layer:RenderType, ccrs:CCRenderState)
    {
        RenderFramedWire.render(this, ccrs)
    }
}

object WireBoxes
{
    var sBounds = Array.ofDim[Cuboid6](3, 6)
    var sShapes = Array.ofDim[VoxelShape](3, 6)
    var oBounds = Array.ofDim[Cuboid6](3, 6)
    var oShapes = Array.ofDim[VoxelShape](3, 6)

    for (t <- 0 until 3) {
        val selection = new Cuboid6(0, 0, 0, 1, (t+2)/16D, 1).expand(-0.005)
        val occlusion = new Cuboid6(2/8D, 0, 2/8D, 6/8D, (t+2)/16D, 6/8D)
        for (s <- 0 until 6) {
            sBounds(t)(s) = selection.copy.apply(Rotation.sideRotations(s).at(Vector3.CENTER))
            sShapes(t)(s) = VoxelShapeCache.getShape(sBounds(t)(s))
            oBounds(t)(s) = occlusion.copy.apply(Rotation.sideRotations(s).at(Vector3.CENTER))
            oShapes(t)(s) = VoxelShapeCache.getShape(oBounds(t)(s))
        }
    }

    var fOBounds = {
        val boxes = new Array[Cuboid6](7)
        val w = 2/8D
        boxes(6) = new Cuboid6(0.5-w, 0.5-w, 0.5-w, 0.5+w, 0.5+w, 0.5+w)
        for (s <- 0 until 6)
            boxes(s) = new Cuboid6(0.5-w, 0, 0.5-w, 0.5+w, 0.5-w, 0.5+w).apply(Rotation.sideRotations(s).at(Vector3.CENTER))
        boxes
    }
    var fOShapes = fOBounds.map(VoxelShapeCache.getShape)
    var fOShapeStates = {
        val shapes = new Array[VoxelShape](64)
        for (m <- 0 until 64) {
            var builder = Seq.newBuilder[VoxelShape].+=(VoxelShapeCache.getShape(fOBounds(6)))
            for (s <- 0 until 6) {
                if ((m & (1 << s)) != 0) builder += VoxelShapeCache.getShape(fOBounds(s))
            }
            shapes(m) = VoxelShapeCache.merge(ImmutableSet.copyOf(builder.result().asJava))
        }
        shapes
    }
    var expandBounds = -1
}
