package mrtjp.projectred.illumination

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.VoxelShapeCache
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec._
import codechicken.multipart.api.MultiPartType
import codechicken.multipart.minecraft.ButtonPart
import codechicken.multipart.util.PartRayTraceResult
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.api.IScrewdriver
import mrtjp.projectred.core.RenderHalo
import net.minecraft.block.{AbstractButtonBlock, Blocks}
import net.minecraft.client.renderer.IRenderTypeBuffer
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.math.shapes.{ISelectionContext, VoxelShape}
import net.minecraft.util.{ActionResultType, Hand}

class LightButtonPart(partType:MultiPartType[_], colour:Int)
        extends ButtonPart(partType, Blocks.STONE_BUTTON.asInstanceOf[AbstractButtonBlock]) with ILight
{
    var inverted = false

    override def activate(player:PlayerEntity, hit:PartRayTraceResult, item:ItemStack, hand:Hand):ActionResultType = {
        if (!item.isEmpty && item.getItem.isInstanceOf[IScrewdriver] && item.getItem.asInstanceOf[IScrewdriver].canUse(player, item)) {
            if (!world.isClientSide) {
                inverted = !inverted
                sendUpdate(writeDesc)
                item.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, item)
            }
            ActionResultType.SUCCESS
        } else
            super.activate(player, hit, item, hand)
    }

    override def isOn:Boolean = pressed != inverted

    override def getColor:Int = colour

    override def save(tag:CompoundNBT) {
        super.save(tag)
        tag.putBoolean("inv", inverted)
    }

    override def load(tag:CompoundNBT) {
        super.load(tag)
        inverted = tag.getBoolean("inv")
    }

    override def writeDesc(packet:MCDataOutput) {
        super.writeDesc(packet)
        packet.writeBoolean(inverted)
    }

    override def readDesc(packet:MCDataInput) {
        super.readDesc(packet)
        inverted = packet.readBoolean
    }

    override def getDropStack:ItemStack = new ItemStack(IlluminationContent.illumarButtonItems(colour).get(), 1)

//    override def drop()
//    {
//        TileMultipart.dropItem(getItemStack, world, Vector3.fromTileCenter(tile))
//        tile.remPart(this)
//    }

//    override def canRenderInLayer(layer:BlockRenderLayer) = false //suppress vanilla rendering of button

//    override def canRenderInLayer(layer:RenderType):Boolean = false


//    override def renderStatic(layer:RenderType, ccrs:CCRenderState):Boolean = {
//        if (layer == RenderType.getSolid) {
//            ccrs.setBrightness(world, this.pos)
//            new IconTransformation(Minecraft.getInstance().getAtlasSpriteGetter(new ResourceLocation("minecraft:blocks/stone")).
//                    .getTextureMapBlocks.getAtlasSprite("minecraft:blocks/stone"))
//        }
//
//    }

    override def renderDynamic(mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float):Unit = {
        if (isOn) {
            val bounds = getBounds.expand(0.025D)
            val ccrs = CCRenderState.instance()
            RenderHalo.prepareRenderState(ccrs, mStack, buffers)
            RenderHalo.renderToCCRS(ccrs, bounds, colour, new RedundantTransformation)
        }
    }

    override def getCollisionShape(context:ISelectionContext):VoxelShape =
        if (tile == null) {
            VoxelShapeCache.getShape(Cuboid6.full)
        } else
            super.getCollisionShape(context)


    //    @SideOnly(Side.CLIENT)
//    override def renderStatic(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState) =
//    {
//        if (layer == BlockRenderLayer.SOLID) {
//            ccrs.setBrightness(world, this.pos)
//            ccrs.setPipeline(pos.translation, new IconTransformation(Minecraft.getMinecraft
//                    .getTextureMapBlocks.getAtlasSprite("minecraft:blocks/stone")),
//                ColourMultiplier.instance(EnumColour.values()(colorMeta).rgba), ccrs.lightMatrix)
//            BlockRenderer.renderCuboid(ccrs, getBounds, 0)
//            true
//        }
//        else false
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def renderDynamic(vec:Vector3, pass:Int, frame:Float)
//    {
//        val box = getBounds.expand(0.025D)
//        RenderHalo.addLight(pos, colorMeta, box)
//    }

//    override def canRenderDynamic(pass: Int) = pass == 0 && isOn

//    @SideOnly(Side.CLIENT)
//    override def getBrokenIcon(side:Int) =
//        TextureUtils.getParticleIconForBlock(Blocks.STAINED_HARDENED_CLAY.getStateFromMeta(colorMeta))

    override def getLightValue:Int = if (isOn) 5 else 0
}

//object LightButtonPart
//{
//    val typeID = new ResourceLocation("projectred-illumination:light_button")
//}

//class FLightButtonPart extends LightButtonPart
//{
//    var powered = false
//
//    override def isOn = powered != inverted
//
//    override def onAdded()
//    {
//        super.onAdded()
//        if (!world.isRemote) checkAndUpdatePower()
//    }
//
//    override def onNeighborChanged()
//    {
//        super.onNeighborChanged()
//        if (world == null) return //might have been dropped in super call
//        if (!world.isRemote) checkAndUpdatePower()
//    }
//
//    def checkAndUpdatePower()
//    {
//        val old = powered
//        powered = isPowered
//        if (old != powered) sendPowUpdate()
//
//        def isPowered:Boolean =
//        {
//            val side = getSideFromState
//            for (s <- 0 until 6) if (s != (side^1))
//                if (RedstoneInteractions.getPowerTo(this, s) > 0)
//                    return true
//            false
//        }
//    }
//
//    override def writeDesc(packet:MCDataOutput)
//    {
//        super.writeDesc(packet)
//        packet.writeBoolean(powered)
//    }
//
//    override def readDesc(packet:MCDataInput)
//    {
//        super.readDesc(packet)
//        powered = packet.readBoolean()
//    }
//
//    def sendPowUpdate() {getWriteStreamOf(3).writeBoolean(powered)}
//
//    override def read(packet:MCDataInput, key:Int) = key match
//    {
//        case 3 => powered = packet.readBoolean()
//        case _ => super.read(packet, key)
//    }
//
//    override def getItem = ProjectRedIllumination.itemPartIllumarFButton
//
//    override def getType = FLightButtonPart.typeID
//}
//
//object FLightButtonPart
//{
//    val typeID = new ResourceLocation("projectred-illumination:feedback_light_button")
//}
//
//trait TButtonItemRendererCommons extends IItemRenderer
//{
//    val invRenderBox = new Cuboid6(5/16D, 6/16D, 6/16D, 11/16D, 10/16D, 10/16D)
//    val invLightBox = invRenderBox.copy.expand(0.025D)
//
//    override def isAmbientOcclusion = true
//    override def isGui3d = true
//    override def getTransforms = TransformUtils.DEFAULT_BLOCK
//
//    override def renderItem(item:ItemStack, transformType: TransformType)
//    {
//        val colour = if (0 until 16 contains item.getItemDamage) item.getItemDamage else 0
//        renderButtonInventory(colour, 0, 0, 0, 1)
//    }
//
//    def renderButtonInventory(colour:Int, x:Float, y:Float, z:Float, scale:Float)
//    {
//        val icon = new IconTransformation(TextureUtils.getParticleIconForBlock(
//            Blocks.STAINED_HARDENED_CLAY.getStateFromMeta(colour)))
//        val t = new Scale(scale) `with` new Translation(x, y, z)
//
//        val ccrs = CCRenderState.instance()
//        TextureUtils.bindBlockTexture()
//        ccrs.reset()
//        ccrs.pullLightmap()
//        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
//
//        ccrs.setPipeline(t, icon, new ColourMultiplier(EnumColour.values()(colour).rgba))
//        BlockRenderer.renderCuboid(ccrs, invRenderBox, 0)
//        drawExtras(ccrs, t)
//
//        ccrs.draw()
//
//        RenderHalo.prepareRenderState()
//        RenderHalo.renderHalo(invLightBox, colour, t)
//        RenderHalo.restoreRenderState()
//    }
//
//    def drawExtras(ccrs:CCRenderState, t:Transformation){}
//}
//
//object ButtonItemRenderer extends TButtonItemRendererCommons
//
//object FButtonItemRenderer extends TButtonItemRendererCommons
//{
//    val model = genModel(10, 8, 8)
//
//    override def drawExtras(ccrs:CCRenderState, t:Transformation)
//    {
//        model.render(ccrs, new Translation(0, 6/16D, 0) `with` t, new IconTransformation(Minecraft.getMinecraft
//                .getTextureMapBlocks.getAtlasSprite("minecraft:blocks/redstone_torch_on")))
//    }
//
//    private def genModel(height:Int, x:Double, z:Double):CCModel =
//    {
//        val m = CCModel.quadModel(20)
//        m.verts(0) = new Vertex5(7/16D, 10/16D, 9/16D, 7/16D, 8/16D)
//        m.verts(1) = new Vertex5(9/16D, 10/16D, 9/16D, 9/16D, 8/16D)
//        m.verts(2) = new Vertex5(9/16D, 10/16D, 7/16D, 9/16D, 6/16D)
//        m.verts(3) = new Vertex5(7/16D, 10/16D, 7/16D, 7/16D, 6/16D)
//        m.generateBlock(4, 6/16D, (10-height)/16D, 7/16D, 10/16D, 11/16D, 9/16D, 0x33)
//        m.generateBlock(12, 7/16D, (10-height)/16D, 6/16D, 9/16D, 11/16D, 10/16D, 0xF)
//        m.apply(new Translation(-0.5+x/16, (height-10)/16D, -0.5+z/16))
//        m.computeNormals
//        m.shrinkUVs(0.0005)
//        m.apply(new Scale(1.0005))
//        m
//    }
//}
