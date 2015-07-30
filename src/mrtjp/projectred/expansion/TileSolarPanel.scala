/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.lighting.LightModel
import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import codechicken.lib.render.{CCModel, CCRenderState, TextureUtils}
import codechicken.lib.vec._
import codechicken.microblock.FaceMicroClass
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart, TMultiPart}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.item.{ItemCore, TItemGlassSound}
import mrtjp.core.world.PlacementLib
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core.{ILowLoadMachine, ILowLoadPowerLine, PowerConductor}
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.util.IIcon
import net.minecraft.world.World
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRenderType, ItemRendererHelper}

import scala.collection.JavaConversions._

class SolarPanelPart extends TMultiPart with TFaceElectricalDevice with ILowLoadMachine
{
    val cond = new PowerConductor(this, 0 until 4)
    {
        override def capacitance = 4.0
    }

    override def getType = "pr_solar"

    override def getItem = new ItemStack(ProjectRedExpansion.itemSolar)

    override def conductor(dir:Int) = cond

    override def getBounds = FaceMicroClass.aBounds(0x10|side)
    override def getOcclusionBoxes = SolarPanelPart.oBoxes(side).toSeq

    override def doesRotate = false

    override def canConnectPart(part:IConnectable, r:Int) = part match
    {
        case t:ILowLoadMachine => true
        case t:ILowLoadPowerLine => true
        case _ => false
    }

    override def update()
    {
        super.update()
        if (!world.isRemote)
        {
            cond.update()
            if (cond.voltage() < 100.0)
            {
                val I = 2.5*heightMultiplier*timeOfDayMultiplier*sideMultiplier*rainMultiplier*visibilityMultiplier
                cond.applyCurrent(I)
            }
        }
    }

    def heightMultiplier = 0.90+0.10*y/256.0

    def timeOfDayMultiplier =
    {
        val t = world.getWorldTime%24000
        if (t > 12000) 0.0
        else 0.50+0.50*math.sin(math.Pi*t/12000.0)
    }

    def sideMultiplier = side match
    {
        case 0 => 1.0
        case 1 => 0.0
        case 2|3 => 0.4
        case 4|5 => 0.3
    }

    def rainMultiplier = 1.0-world.rainingStrength

    def visibilityMultiplier =
        if (tile.partMap(1) != null) 0.0
        else if (world.canBlockSeeTheSky(x, y, z)) 1.0
        else if (world.canBlockSeeTheSky(x, y+1, z) && !world.getBlock(x, y+1, z).getMaterial.isOpaque) 0.7
        else 0.0

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, pass:Int) =
    {
        if (pass == 0)
        {
            TextureUtils.bindAtlas(0)
            CCRenderState.setBrightness(world, x, y, z)
            RenderSolarPanel.render(side, pos)
            true
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def getBrokenIcon(side:Int) =
        if (side == 1) RenderSolarPanel.top else RenderSolarPanel.side
}

object SolarPanelPart
{
    var oBoxes = Array.ofDim[Cuboid6](6, 2)

    oBoxes(0)(0) = new Cuboid6(1 / 8D, 0, 0, 7 / 8D, 1 / 8D, 1)
    oBoxes(0)(1) = new Cuboid6(0, 0, 1 / 8D, 1, 1 / 8D, 7 / 8D)
    for (s <- 1 until 6)
    {
        val t = Rotation.sideRotations(s).at(Vector3.center)
        oBoxes(s)(0) = oBoxes(0)(0).copy.apply(t)
        oBoxes(s)(1) = oBoxes(0)(1).copy.apply(t)
    }
}

class ItemSolarPanel extends ItemCore("projectred.expansion.solar_panel") with TItemMultiPart with TItemGlassSound
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3):TMultiPart =
    {
        val onPos = pos.copy.offset(side^1)
        if (!PlacementLib.canPlaceGateOnSide(world, onPos.x, onPos.y, onPos.z, side)) return null

        val solar = MultiPartRegistry.createPart("pr_solar", false).asInstanceOf[SolarPanelPart]
        if (solar != null) solar.preparePlacement(player, pos, side, item.getItemDamage)
        solar

    }

    override def registerIcons(reg:IIconRegister)
    {
        RenderSolarPanel.registerIcons(reg)
    }

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0
}

object RenderSolarPanel extends IItemRenderer
{
    var side:IIcon = null
    var top:IIcon = null
    var bottom:IIcon = null

    var iconT:UVTransformation = null

    val models =
    {
        val array = new Array[CCModel](6)
        val m = CCModel.quadModel(24)
        m.generateBlock(0,  new Cuboid6(0, 0, 0, 1, 2/16D, 1).expand(-0.0005), 0)
        for (s <- 0 until 6)
        {
            val m2 = m.copy.apply(Rotation.sideRotations(s) at Vector3.center)
            m2.computeNormals()
            m2.shrinkUVs(0.0005)
            m2.computeLighting(LightModel.standardLightModel)
            array(s) = m2
        }
        array
    }

    def render(side:Int, pos:Vector3)
    {
        models(side).render(iconT, pos.translation)
    }

    def registerIcons(reg:IIconRegister)
    {
        side = reg.registerIcon("projectred:mechanical/solar/side")
        top = reg.registerIcon("projectred:mechanical/solar/top")
        bottom = reg.registerIcon("projectred:mechanical/solar/bottom")
        iconT = new MultiIconTransformation(bottom, top, side, side, side, side)
    }

    override def shouldUseRenderHelper(t:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true
    override def handleRenderType(item:ItemStack, t:ItemRenderType) = true

    override def renderItem(t:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.setDynamic()
        CCRenderState.pullLightmap()
        CCRenderState.startDrawing()
        t match
        {
            case ItemRenderType.INVENTORY => render(-0.5, -0.5, -0.5, 1)
            case ItemRenderType.ENTITY => render(-0.5, -0.5, -0.5, 0.5)
            case ItemRenderType.EQUIPPED => render(0, 0.5, 0, 1)
            case ItemRenderType.EQUIPPED_FIRST_PERSON => render(0, 0.5, 0, 1)
            case _ =>
        }
        CCRenderState.draw()

        def render(x:Double, y:Double, z:Double, scale:Double)
        {
            models(0).render(new Scale(scale) at Vector3.center `with` new Translation(x, y, z), iconT)
        }
    }
}