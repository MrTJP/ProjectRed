package mrtjp.projectred.illumination

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.VoxelShapeCache
import codechicken.lib.render._
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.lighting.LightModel
import codechicken.lib.render.pipeline.ColourMultiplier
import codechicken.lib.texture.AtlasRegistrar
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec.uv.IconTransformation
import codechicken.lib.vec.{Rotation, _}
import codechicken.microblock.HollowMicroblock
import codechicken.multipart.api.part.redstone.{IMaskedRedstonePart, IRedstonePart}
import codechicken.multipart.api.part.{TFacePart, TMultiPart, TNormalOcclusionPart, TSlottedPart}
import codechicken.multipart.api.{MultiPartType, RedstoneInteractions, SimpleMultiPartType}
import codechicken.multipart.block.{BlockMultiPart, TileMultiPart}
import codechicken.multipart.util.PartRayTraceResult
import com.google.common.collect.ImmutableMap
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.vec.InvertX
import mrtjp.projectred.core.{Configurator, PRLib, RenderHalo}
import net.minecraft.block.SoundType
import net.minecraft.client.renderer.model.ItemCameraTransforms
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType, TransformationMatrix}
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.{Item, ItemStack, ItemUseContext}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.shapes.VoxelShape
import net.minecraft.util.{Direction, ResourceLocation}
import net.minecraft.world.World
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.fml.RegistryObject
import net.minecraftforge.registries.DeferredRegister

import scala.jdk.CollectionConverters._

class BaseLightPart(definition:LightPartDefinition, colour:Int, inverted:Boolean) extends TMultiPart with TSlottedPart with TNormalOcclusionPart with IRedstonePart with ILight
{
    protected var powered = false
    protected var side:Byte = 0

    def getColor:Int = colour

    def getSide:Int = side&0xF

    def isInverted:Boolean = inverted

    def preparePlacement(side:Int) {
        this.side = (side&0xF).toByte
    }

    override def save(tag:CompoundNBT)
    {
        tag.putBoolean("pow", powered)
        tag.putByte("side", side)
    }

    override def load(tag:CompoundNBT)
    {
        powered = tag.getBoolean("pow")
        side = tag.getByte("side")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(side).writeBoolean(powered)
    }

    override def readDesc(packet:MCDataInput)
    {
        side = packet.readByte()
        powered = packet.readBoolean()
    }

    override def readUpdate(packet:MCDataInput):Unit = {
        readDesc(packet)
        updateRender()
    }

    /**
     * Called when a neighbor block changed
     */
    override def onNeighborBlockChanged(from:BlockPos):Unit = {
        if (checkSupport) return
        updateState(false)
    }

    def checkSupport:Boolean =
    {
        if (world.isRemote) return false
        if (definition.canFloat) return false
        val bc = pos.offset(Direction.byIndex(getSide))

        if (!definition.canFloat && !BaseLightPart.canPlaceLight(world, bc, Direction.byIndex(getSide^1))) {
            TileMultiPart.dropItem(getItem, world, Vector3.fromTileCenter(tile))
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
        if (!world.isRemote) sendUpdate(writeDesc)
        tile.recalcLight(false, true)
        tile.markRender()
    }

    override def getLightValue:Int = if (isInverted != powered) 15 else 0

    @OnlyIn(Dist.CLIENT)
    override def renderStatic(layer:RenderType, ccrs:CCRenderState):Boolean = {
        if (layer == null || (layer == RenderType.getCutout && Configurator.staticGates)) {
            ccrs.setBrightness(world, this.pos)
            definition.render(this, Vector3.ZERO,ccrs)
            true
        } else
            false
    }

    @OnlyIn(Dist.CLIENT)
    override def renderDynamic(mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float):Unit = {
//        if (isOn)
//            RenderHalo.addLight(pos, getColor, getLightBounds) //TODO RenderWorldLastEvent rendering is broken
        if (isOn)
            RenderHalo.renderHalo(CCRenderState.instance(), mStack, buffers, definition.getGlowBounds(getSide), colour, Vector3.ZERO)
    }

    def getItem:ItemStack = definition.makeStack(colour, inverted)

    def getLightBounds:Cuboid6 = definition.getGlowBounds(getSide)

    override def getOutlineShape:VoxelShape = definition.getShape(getSide)
    override def getOcclusionShape:VoxelShape = getOutlineShape

    override def getType:MultiPartType[_] = definition.multiPartType(colour, inverted)

    override def getStrength(player:PlayerEntity, hit:PartRayTraceResult):Float = 2/30f
    override def getSlotMask:Int = 1<<6

    override def getDrops:java.lang.Iterable[ItemStack] = Seq(getItem).asJava
    override def pickItem(hit:PartRayTraceResult):ItemStack = getItem

    override def canConnectRedstone(side:Int) = true
    override def strongPowerLevel(side:Int) = 0
    override def weakPowerLevel(side:Int) = 0

    override def isOn:Boolean = powered != isInverted

    override def getPlacementSound(context:ItemUseContext):SoundType = SoundType.GLASS
}

object BaseLightPart
{
    def canPlaceLight(w:World, pos:BlockPos, side:Direction):Boolean =
    {
        if (PRLib.canPlaceLight(w, pos, side)) return true

        val part = BlockMultiPart.getPart(w, pos, side.getIndex)
        if (part.isInstanceOf[HollowMicroblock]) return true

        false
    }
}

class BaseLightFacePart(definition:LightPartDefinition, colour:Int, inverted:Boolean) extends BaseLightPart(definition, colour, inverted) with TFacePart with IMaskedRedstonePart
{
    override def solid(side:Int) = false
    override def getSlotMask:Int = (1<<getSide)&0x40

    override def getConnectionMask(s:Int):Int = {
        if ((s^1) == getSide) 0
        else if (s == getSide) 0x10
        else 1<<Rotation.rotationTo(s&6, getSide)
    }
}

//trait TAirousLight extends BaseLightPart with ITickable
//{
//    abstract override def update()
//    {
//        super.update()
//        if (!world.isRemote && isOn) {
//            val rad = lightRadius
//
//            val pos1 = pos.add(
//                world.rand.nextInt(rad)-world.rand.nextInt(rad),
//                Math.max(Math.min(pos.getY+world.rand.nextInt(rad)-world.rand.nextInt(rad), world.getHeight(pos).getY+4), 7),
//                world.rand.nextInt(rad)-world.rand.nextInt(rad)
//            )
//
//            if (world.isAirBlock(pos1) && world.getLightFor(EnumSkyBlock.BLOCK, pos1) < 8) {
//                world.setBlockState(pos, ProjectRedIllumination.blockAirousLight.getDefaultState, 3)
//                world.getTileEntity(pos) match {
//                    case al:TileAirousLight => al.setSource(pos, getColor, getSide)
//                    case _ =>
//                }
//            }
//        }
//    }
//
//    def lightRadius = 16
//}

trait LightPartDefinition {

    val itemRegObjects = new Array[RegistryObject[Item]](16)
    val invertedItemRegObjects = new Array[RegistryObject[Item]](16)

    val partRegObjects = new Array[RegistryObject[MultiPartType[_]]](16)
    val invertedPartRegObjects = new Array[RegistryObject[MultiPartType[_]]](16)

    private val items = new Array[Item](16)
    private val invertedItems = new Array[Item](16)

    private val multiPartTypes = new Array[MultiPartType[_]](16)
    private val invertedMultiPartTypes = new Array[MultiPartType[_]](16)

    def register(itemRegistry:DeferredRegister[Item], partRegistry:DeferredRegister[MultiPartType[_]]):Unit = {
        for (inverted <- Seq(false, true)) for (i <- 0 until 16) {
            val regName = registrationName(i, inverted)
            val itemReg = if (inverted) invertedItemRegObjects else itemRegObjects
            val partReg = if (inverted) invertedPartRegObjects else partRegObjects

            itemReg(i) = itemRegistry.register(regName, () => itemFactory(i, inverted))
            partReg(i) = partRegistry.register(regName, () =>
                new SimpleMultiPartType[TMultiPart](_ => partFactory(i, inverted))
            )
        }
    }

    protected def registrationName(colour:Int, inverted:Boolean):String =
        s"${EnumColour.values()(colour).getName}${ if (inverted) "_inverted" else ""}_${typeName}"

    def multiPartType(colour:Int, inverted:Boolean):MultiPartType[_] = {
        val partArray = if (inverted) invertedMultiPartTypes else multiPartTypes
        val partRegArray = if (inverted) invertedPartRegObjects else partRegObjects
        val partType = partArray(colour)
        if (partType == null) {
            val newPartType = partRegArray(colour).get()
            partArray(colour) = newPartType
            newPartType
        } else
            partType
    }

    def makeStack(colour:Int, inverted:Boolean):ItemStack = {
        val itemArray = if (inverted) invertedItems else items
        val itemRegArray = if (inverted) invertedItemRegObjects else itemRegObjects
        val item = itemArray(colour)
        if (item == null) {
            val newItem = itemRegArray(colour).get()
            itemArray(colour) = newItem
            new ItemStack(newItem)
        } else
            new ItemStack(item)
    }

    // Basic registration and factories

    protected def typeName:String
    protected def itemFactory(colour:Int, inverted:Boolean):Item
    protected def partFactory(colour:Int, inverted:Boolean):TMultiPart

    // Client registration
    @OnlyIn(Dist.CLIENT)
    def getItemModelPath:String //Path to the json file containing the loader location

    @OnlyIn(Dist.CLIENT)
    def getItemModelLoaderPath:String //Path of the loader inside above json file

    @OnlyIn(Dist.CLIENT)
    def registerIcons(registrar:AtlasRegistrar):Unit

    @OnlyIn(Dist.CLIENT)
    def loadModels():Unit

    @OnlyIn(Dist.CLIENT)
    def getItemRenderer:IItemRenderer = new IItemRenderer {
        override def isAmbientOcclusion:Boolean = true
        override def isGui3d:Boolean = true
        override def func_230044_c_():Boolean = true

        override def getTransforms:ImmutableMap[ItemCameraTransforms.TransformType, TransformationMatrix] = TransformUtils.DEFAULT_BLOCK

        override def renderItem(stack:ItemStack,
                                transformType:ItemCameraTransforms.TransformType,
                                mStack:MatrixStack,
                                getter:IRenderTypeBuffer,
                                packedLight:Int,
                                packedOverlay:Int):Unit = {
            stack.getItem match {
                case light:ItemBaseLight =>
                    val ccrs = CCRenderState.instance()
                    ccrs.reset()
                    ccrs.brightness = packedLight
                    ccrs.overlay = packedOverlay
                    ccrs.bind(RenderType.getCutout, getter, mStack)
                    renderInv(light.colour, light.inverted, Vector3.ZERO, ccrs)

                    if (light.inverted) {
                        RenderHalo.renderHalo(ccrs, mStack, getter, getInvGlowBounds, light.colour, Vector3.ZERO)
                    }
                case _ =>
            }
        }
    }

    // Part properties and behaviours

    def canFloat = false

    def getShape(side:Int):VoxelShape
    def getGlowBounds(side:Int):Cuboid6

    // Rendering
    def getIcon(colour:Int):TextureAtlasSprite
    def getModelBulb(side:Int):CCModel
    def getModelChassi(side:Int):CCModel

    def getInvModelBulb:CCModel = getModelBulb(0)
    def getInvModelChassi:CCModel = getModelChassi(0)
    def getInvGlowBounds:Cuboid6 = getGlowBounds(0)

    @OnlyIn(Dist.CLIENT)
    def getRenderLayer:RenderType = RenderType.getSolid

    def render(part:BaseLightPart, pos:Vector3, ccrs:CCRenderState):Unit = {
        val icon = new IconTransformation(getIcon(part.getColor))
        val t = pos.translation()
        getModelChassi(part.getSide).render(ccrs, t, icon)
        getModelBulb(part.getSide).render(ccrs, t, icon, cMult(part.getColor, part.isOn))
    }

    def renderInv(colour:Int, inverted:Boolean, pos:Vector3, ccrs:CCRenderState):Unit = {
        val icon = new IconTransformation(getIcon(colour))
        val t = new Translation(pos)

        //Render
        getInvModelChassi.render(ccrs, t, icon)
        getInvModelBulb.render(ccrs, t, icon, cMult(colour, inverted))
    }

    def cMult(color:Int, on:Boolean):ColourMultiplier = {
        val c = EnumColour.values()(color).getColour
        if (!on) c.multiply(EnumColour.LIGHT_GRAY.getColour)
        ColourMultiplier.instance(c.rgba)
    }
}

object LightPartDefinition {

    def sideRotationBounds(box:Cuboid6):Array[Cuboid6] = {
        val boxes = new Array[Cuboid6](6)
        boxes(0) = box.copy
        for (s <- 1 until 6)
            boxes(s) = box.copy.apply(Rotation.sideRotations(s).at(Vector3.CENTER))
        boxes
    }

    def sideRotationShapes(bounds:Array[Cuboid6]):Array[VoxelShape] =
        bounds.map(VoxelShapeCache.getShape)

    @deprecated
    def parseModel(name:String) = {
        val models = OBJParser.parseModels(
            new ResourceLocation("projectred", "textures/obj/lighting/"+name+".obj"), 7, InvertX)
        for (m <- models.asScala.values) m.apply(new Translation(0.5, 0, 0.5))
        models.asScala
    }

    def parseCorrectedModel(name:String) = {
        val models = OBJParser.parseModels(
            new ResourceLocation("projectred", "textures/obj/lighting/"+name+".obj"), 7, null)

        models.asScala.map(m => m._1 -> m._2.backfacedCopy().apply(new Translation(0.5, 0, 0.5)))
    }

    def bakeCopy(s:Int, m1:CCModel):CCModel = {
        val m = m1.copy
        m.apply(Rotation.sideOrientation(s, 0).at(Vector3.CENTER))
        finishModel(m)
        m
    }

    def finishModel(m:CCModel):CCModel = {
        m.computeNormals()
        m.computeLighting(LightModel.standardLightModel)
        m.shrinkUVs(0.0005)
    }

    def sidedBoxes(box:Cuboid6):Array[Cuboid6] = {
        val boxes = new Array[Cuboid6](6)
        boxes(0) = box.copy
        for (s <- 1 until 6)
            boxes(s) = box.copy.apply(Rotation.sideRotations(s).at(Vector3.CENTER))
        boxes
    }
}
