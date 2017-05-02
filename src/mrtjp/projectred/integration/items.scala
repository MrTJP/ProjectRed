/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import java.util.{List => JList}

import codechicken.lib.render.CCRenderState
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec._
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart, TMultiPart}
import com.google.common.collect.ImmutableList
import mrtjp.core.item.{ItemCore, ItemDefinition}
import mrtjp.projectred.ProjectRedIntegration
import mrtjp.projectred.core.libmc.PRLib
import net.minecraft.block.SoundType
import net.minecraft.block.state.IBlockState
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.block.model.{ItemCameraTransforms, ItemOverrideList}
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.client.model.IPerspectiveAwareModel
import net.minecraftforge.client.model.IPerspectiveAwareModel.MapWrapper
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.opengl.GL11

class ItemPartGate extends ItemCore with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedIntegration.tabIntegration)
    var infoBuilderFunc = {(stack:ItemStack, l:JList[String]) => }

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, side:Int, vhit:Vector3):TMultiPart =
    {
        val onPos = pos.offset(EnumFacing.values()(side^1))
        if (!PRLib.canPlaceGateOnSide(world, onPos, side)) return null

        val gtype = GateDefinition(item.getItemDamage)
        if (!gtype.implemented) return null

        val gate = MultiPartRegistry.loadPart(gtype.partname, null).asInstanceOf[GatePart]
        if (gate != null) gate.preparePlacement(player, pos, side, item.getItemDamage)
        gate
    }

    override def getPlacementSound(item:ItemStack) = SoundType.GLASS

    @SideOnly(Side.CLIENT)
    override def getSubItems(id:Item, tab:CreativeTabs, list:JList[ItemStack])
    {
        for (g <- GateDefinition.values)
            if (g.implemented) list.add(g.makeStack)
    }

//    override def registerIcons(reg:IIconRegister)
//    {
//        ComponentStore.registerIcons(reg)
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def getSpriteNumber = 0
//

    override def addInformation(stack:ItemStack, player:EntityPlayer, tooltip:JList[String], advanced:Boolean)
    {
        infoBuilderFunc(stack, tooltip)
    }
}

object GateDefinition extends ItemDefinition
{
    override type EnumVal = GateDef
    override def getItem = ProjectRedIntegration.itemPartGate

    val typeSimpleGate = "projectred-integration:simple_gate"
    val typeComplexGate = "projectred-integration:complex_gate"
    val typeArrayGate = "projectred-integration:array_gate"
    val typeBundledGate = "projectred-integration:bundled_gate"
    val typeNeighborGate = "projectred-integration:neighbor_gate"
    val typeICGate = "projectred-fabrication:ic_gate" //used by fabrication module

    val OR = new GateDef(typeSimpleGate)
    val NOR = new GateDef(typeSimpleGate)
    val NOT = new GateDef(typeSimpleGate)
    val AND = new GateDef(typeSimpleGate)
    val NAND = new GateDef(typeSimpleGate)
    val XOR = new GateDef(typeSimpleGate)
    val XNOR = new GateDef(typeSimpleGate)
    val Buffer = new GateDef(typeSimpleGate)
    val Multiplexer = new GateDef(typeSimpleGate)
    val Pulse = new GateDef(typeSimpleGate)
    val Repeater = new GateDef(typeSimpleGate)
    val Randomizer = new GateDef(typeSimpleGate)
    val SRLatch = new GateDef(typeComplexGate)
    val ToggleLatch = new GateDef(typeComplexGate)
    val TransparentLatch = new GateDef(typeSimpleGate)
    val LightSensor = new GateDef(typeSimpleGate)
    val RainSensor = new GateDef(typeSimpleGate)
    val Timer = new GateDef(typeComplexGate)
    val Sequencer = new GateDef(typeComplexGate)
    val Counter = new GateDef(typeComplexGate)
    val StateCell = new GateDef(typeComplexGate)
    val Synchronizer = new GateDef(typeComplexGate)
    val BusTransceiver = new GateDef(typeBundledGate)
    val NullCell = new GateDef(typeArrayGate)
    val InvertCell = new GateDef(typeArrayGate)
    val BufferCell = new GateDef(typeArrayGate)
    val Comparator = new GateDef(typeNeighborGate)
    val ANDCell = new GateDef(typeArrayGate)
    val BusRandomizer = new GateDef(typeBundledGate)
    val BusConverter = new GateDef(typeBundledGate)
    val BusInputPanel = new GateDef(typeBundledGate)
    val StackingLatch = new GateDef(typeArrayGate)
    val SegmentDisplay = new GateDef(typeBundledGate)
    val DecRandomizer = new GateDef(typeSimpleGate)
    val ICGate = new GateDef(typeICGate, true) //fabrication module

    class GateDef(val partname:String, val hidden:Boolean = false) extends ItemDef(partname)
    {
        def implemented = partname != null
    }
}

object GateItemRenderer extends IItemRenderer with IPerspectiveAwareModel
{
    override def isBuiltInRenderer = true
    override def getParticleTexture = null
    override def getItemCameraTransforms = ItemCameraTransforms.DEFAULT
    override def isAmbientOcclusion = true
    override def isGui3d = true
    override def getOverrides = ItemOverrideList.NONE
    override def getQuads(state:IBlockState, side:EnumFacing, rand:Long) = ImmutableList.of()

    override def handlePerspective(t:TransformType) =
        MapWrapper.handlePerspective(this, TransformUtils.DEFAULT_BLOCK, t)

    override def renderItem(item:ItemStack)
    {
        val meta = item.getItemDamage
        if (!GateDefinition.values.isDefinedAt(meta) ||
                !GateDefinition(meta).implemented) return

        import net.minecraft.client.renderer.GlStateManager._

        enableBlend()
        blendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

        TextureUtils.bindBlockTexture()
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.pullLightmap()
        RenderGate.renderInv(item, new RedundantTransformation, item.getItemDamage, ccrs)

        disableBlend()
    }
}
