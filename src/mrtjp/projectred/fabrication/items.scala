/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import java.util.{List => JList}

import codechicken.lib.gui.GuiDraw
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.core.color.Colors
import mrtjp.core.item.ItemCore
import mrtjp.core.vec.{Point, Size}

import mrtjp.projectred.{ProjectRedIntegration, ProjectRedFabrication}
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.Entity
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{ItemMap, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{IIcon, EnumChatFormatting}
import net.minecraft.world.World
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRenderType, ItemRendererHelper}
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11._

class ItemICBlueprint extends ItemMap //extend ItemMap so minecraft will handle the FP render for us
{
    setMaxStackSize(1)
    setCreativeTab(ProjectRedFabrication.tabFabrication)
    setUnlocalizedName("projectred.fabrication.icblueprint")
    GameRegistry.registerItem(this, "projectred.fabrication.icblueprint")
    setTextureName("projectred:icblueprint")

    //no need for standard map code to run.
    override def onUpdate(stack:ItemStack, world:World, player:Entity, a:Int, b:Boolean){}
    override def func_150911_c(stack:ItemStack, world:World, player:EntityPlayer) = null
    override def onCreated(stack:ItemStack, world:World, player:EntityPlayer){}

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[_], par4:Boolean)
    {
        import EnumChatFormatting._
        val slist = list.asInstanceOf[JList[String]]

        if (ItemICBlueprint.hasICInside(stack))
        {
            val size = ItemICBlueprint.getICSize(stack)
            slist.add(GRAY+ItemICBlueprint.getICName(stack))
            slist.add(GRAY+s"${size.width} x ${size.height}")
        }
        else slist.add(GRAY+"empty blueprint")
    }
}

object ItemICBlueprint
{
    var blueprintItems = Seq(
        ProjectRedFabrication.itemICBlueprint,
        ProjectRedIntegration.itemPartGate2
    )

    def assertStackTag(stack:ItemStack)
    {
        if (!stack.hasTagCompound) stack.setTagCompound(new NBTTagCompound)
    }

    def saveIC(ic:IntegratedCircuit, stack:ItemStack)
    {
        assertStackTag(stack)
        val tag1 = stack.getTagCompound
        val tag2 = new NBTTagCompound
        ic.save(tag2)
        tag1.setTag("icdata", tag2)
        tag1.setString("icname", ic.name)
        tag1.setByte("icw", ic.size.width.toByte)
        tag1.setByte("ich", ic.size.height.toByte)
    }

    def loadIC(stack:ItemStack):IntegratedCircuit =
    {
        val ic = new IntegratedCircuit
        loadIC(ic, stack)
        ic
    }

    def loadIC(ic:IntegratedCircuit, stack:ItemStack)
    {
        val tag = stack.getTagCompound
        if (tag.hasKey("icdata"))
            ic.load(tag.getCompoundTag("icdata"))
    }

    def getICName(stack:ItemStack) =
    {
        stack.getTagCompound.getString("icname")
    }

    def getICSize(stack:ItemStack) =
    {
        val tag = stack.getTagCompound
        Size(tag.getByte("icw"), tag.getByte("ich"))
    }

    def hasICInside(stack:ItemStack) =
    {
        stack.hasTagCompound && stack.getTagCompound.hasKey("icdata")
    }

    def copyIC(from:ItemStack, to:ItemStack)
    {
        assertStackTag(to)

        val totag = to.getTagCompound
        val fromtag = from.getTagCompound
        totag.setTag("icdata", fromtag.getCompoundTag("icdata"))
        totag.setString("icname", fromtag.getString("icname"))
        totag.setByte("icw", totag.getByte("icw"))
        totag.setByte("ich", totag.getByte("ich"))
    }

    def removeIC(stack:ItemStack)
    {
        if (!stack.hasTagCompound) return
        val tag = stack.getTagCompound
        Seq("icdata", "icname", "icw", "ich").foreach(tag.removeTag)
    }

    def copyToGate(bp:ItemStack, gate:ItemStack)
    {
        import mrtjp.projectred.fabrication.IIOCircuitPart._
        assertStackTag(gate)
        val ic = loadIC(bp)

        val ioparts = ic.parts.values.collect{case io:IIOCircuitPart => io}.toSeq

        var (ri, ro, bi, bo) = (0, 0, 0, 0)
        val connmodes = new Array[Int](4)

        for (r <- 0 until 4)
        {
            val sparts = ioparts.filter(_.getIOSide == r)

            val ioMode = if (sparts.exists(_.getIOMode == InOut)) InOut
            else
            {
                val in = sparts.exists(_.getIOMode == Input)
                val out = sparts.exists(_.getIOMode == Output)
                if (in && out) InOut
                else if (in) Input
                else if (out) Output
                else Closed
            }

            val connMode = if (sparts.exists(_.getConnMode == Simple)) Simple
            else if (sparts.exists(_.getConnMode == Analog)) Analog
            else if (sparts.exists(_.getConnMode == Bundled)) Bundled
            else NoConn

            connmodes(r) = connMode
            (ioMode, connMode) match
            {
                case (Input, Simple)    => ri |= 1<<r
                case (Input, Analog)    => ri |= 1<<r
                case (Input, Bundled)   => bi |= 1<<r

                case (Output, Simple)   => ro |= 1<<r
                case (Output, Analog)   => ro |= 1<<r
                case (Output, Bundled)  => bo |= 1<<r

                case (InOut, Simple)    => ri |= 1<<r; ro |= 1<<r
                case (InOut, Analog)    => ri |= 1<<r; ro |= 1<<r
                case (InOut, Bundled)   => bi |= 1<<r; bo |= 1<<r
                case _ =>
            }
        }

        val tag = gate.getTagCompound
        copyIC(bp, gate)
        tag.setShort("masks", CircuitGateLogic.packIO(ri, ro, bi, bo).toShort)
        tag.setShort("cmode", CircuitGateLogic.packConnModes(connmodes).toShort)
    }

    def getGateMasks(stack:ItemStack) =
        CircuitGateLogic.unpackIO(stack.getTagCompound.getShort("masks"))

    def getConnModes(stack:ItemStack) =
        CircuitGateLogic.unpackConnModes(stack.getTagCompound.getShort("cmode"))
}

object ItemRenderICBlueprint extends IItemRenderer
{
    override def handleRenderType(item:ItemStack, t:ItemRenderType) = t == ItemRenderType.FIRST_PERSON_MAP
    override def shouldUseRenderHelper(t:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    override def renderItem(t:ItemRenderType, stack:ItemStack, data:AnyRef*)
    {
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)

        overlayMapTexture()

        if (ItemICBlueprint.hasICInside(stack))
        {
            val ic = ItemICBlueprint.loadIC(stack)
            if (ic.nonEmpty) overlayIC(ic)
        }

        glEnable(GL_LIGHTING)
        glEnable(GL_DEPTH_TEST)
    }

    private def overlayMapTexture()
    {
        import net.minecraft.client.renderer.Tessellator.{instance => tes}
        PRResources.icmaptex.bind()

        val b0 = 7
        tes.startDrawingQuads()
        tes.addVertexWithUV(0 - b0, 128 + b0, 0.0D, 0.0D, 1.0D)
        tes.addVertexWithUV(128 + b0, 128 + b0, 0.0D, 1.0D, 1.0D)
        tes.addVertexWithUV(128 + b0, 0 - b0, 0.0D, 1.0D, 0.0D)
        tes.addVertexWithUV(0 - b0, 0 - b0, 0.0D, 0.0D, 0.0D)
        tes.draw()
    }

    private def overlayIC(ic:IntegratedCircuit)
    {
        val sf = 128/math.max(ic.size.width, ic.size.height)
        val rs = ic.size*sf
        val rp = Point(Size(128, 128)/2-rs/2)
        RenderCircuit.renderOrtho(ic, rp.x, rp.y, rs.width, rs.height, 0)

        val name = ic.name

        glPushMatrix()
        glTranslated(0, 128, 0)
        glScaled(0.55, 0.55, 1)

        GuiDraw.drawRect(0, 0, GuiDraw.getStringWidth(name)+4, 11, 0x5F000000)
        GuiDraw.drawString(name, 2, 2, Colors.WHITE.argb, false)

        glPopMatrix()
    }
}

class ItemICChip extends ItemCore("projectred.fabrication.icchip")
{
    setMaxStackSize(1)
    setCreativeTab(ProjectRedFabrication.tabFabrication)
    setTextureName("projectred:icblueprint")

    var icons = new Array[IIcon](2)

    override def registerIcons(reg:IIconRegister)
    {
        icons(0) = reg.registerIcon("projectred:ic_inert")
        icons(1) = reg.registerIcon("projectred:ic")
    }

    override def getIcon(stack:ItemStack, pass:Int) = getIconIndex(stack)

    override def getIconIndex(stack:ItemStack) =
        icons(if (ItemICBlueprint.hasICInside(stack)) 1 else 0)

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[_], par4:Boolean)
    {
        import EnumChatFormatting._
        val slist = list.asInstanceOf[JList[String]]

        if (ItemICBlueprint.hasICInside(stack)) slist.add(GRAY+ItemICBlueprint.getICName(stack))
    }
}