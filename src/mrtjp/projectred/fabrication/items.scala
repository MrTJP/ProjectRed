/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import java.util.{List => JList}

import codechicken.lib.colour.EnumColour
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.item.map.IMapRenderer
import codechicken.lib.texture.TextureUtils
import com.mojang.realmsclient.gui.ChatFormatting
import mrtjp.core.item.ItemCore
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.fabrication.IIOCircuitPart._
import mrtjp.projectred.integration.GateDefinition
import mrtjp.projectred.{ProjectRedFabrication, ProjectRedIntegration}
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util._
import net.minecraft.world.World
import net.minecraft.world.storage.MapData
import org.lwjgl.opengl.GL11._

class ItemICBlueprint extends Item
{
    setMaxStackSize(1)
    setCreativeTab(ProjectRedFabrication.tabFabrication)

    override def addInformation(stack:ItemStack, playerIn:EntityPlayer, tooltip:JList[String], advanced:Boolean)
    {
        import ChatFormatting._

        if (ItemICBlueprint.hasICInside(stack))
        {
            val size = ItemICBlueprint.getICSize(stack)
            tooltip.add(GRAY+ItemICBlueprint.getICName(stack))
            tooltip.add(GRAY+s"${size.width} x ${size.height}")
        }
        else tooltip.add(GRAY+"empty blueprint")
    }
}

object ItemICBlueprint
{
    var blueprintItems = Seq(
        ProjectRedFabrication.itemICBlueprint,
        ProjectRedIntegration.itemPartGate
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
        totag.setByte("icw", fromtag.getByte("icw"))
        totag.setByte("ich", fromtag.getByte("ich"))
    }

    def removeIC(stack:ItemStack)
    {
        if (!stack.hasTagCompound) return
        val tag = stack.getTagCompound
        Seq("icdata", "icname", "icw", "ich").foreach(tag.removeTag)
    }

    def saveICToGate(ic:IntegratedCircuit, gate:ItemStack)
    {
        assertStackTag(gate)

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
        saveIC(ic, gate)
        tag.setShort("masks", CircuitGateLogic.packIO(ri, ro, bi, bo).toShort)
        tag.setShort("cmode", CircuitGateLogic.packConnModes(connmodes).toShort)
    }

    def copyToGate(bp:ItemStack, gate:ItemStack)
    {
        assertStackTag(gate)
        val ic = loadIC(bp)
        saveICToGate(ic, gate)
    }

    def getGateMasks(stack:ItemStack) =
        CircuitGateLogic.unpackIO(stack.getTagCompound.getShort("masks"))

    def getConnModes(stack:ItemStack) =
        CircuitGateLogic.unpackConnModes(stack.getTagCompound.getShort("cmode"))
}

object ItemRenderICBlueprint extends IMapRenderer
{
    val background = new ResourceLocation("projectred", "textures/gui/map_background.png")

    override def shouldHandle(stack:ItemStack, data:MapData, inFrame:Boolean) = true

    override def renderMap(stack:ItemStack, data:MapData, inFrame:Boolean)
    {
        import net.minecraft.client.renderer.GlStateManager._
        disableLighting()
        disableDepth()

        val ccrs = CCRenderState.instance()
        ccrs.startDrawing(GL_QUADS, DefaultVertexFormats.POSITION_TEX)

        overlayMapTexture(ccrs)

        if (ItemICBlueprint.hasICInside(stack))
        {
            val ic = ItemICBlueprint.loadIC(stack)
            if (ic.nonEmpty) overlayIC(ccrs, ic)
        }

        enableLighting()
        enableDepth()
    }

    private def overlayMapTexture(ccrs:CCRenderState)
    {
        TextureUtils.changeTexture(background)
        val buffer = ccrs.getBuffer

        val b0 = 7
        buffer.pos(0 - b0, 128 + b0, 0.0D).tex(0, 1).endVertex()
        buffer.pos(128 + b0, 128 + b0, 0.0D).tex(1, 1).endVertex()
        buffer.pos(128 + b0, 0 - b0, 0.0D).tex(1, 0).endVertex()
        buffer.pos(0 - b0, 0 - b0, 0.0D).tex(0, 0).endVertex()

        ccrs.draw()
    }

    private def overlayIC(ccrs:CCRenderState, ic:IntegratedCircuit)
    {
        val sf = 128/scala.math.max(ic.size.width, ic.size.height)
        val rs = ic.size*sf
        val rp = Point(Size(128, 128)/2-rs/2)
        RenderCircuit.renderOrtho(ccrs, ic, rp.x, rp.y, rs.width, rs.height, 0)

        val name = ic.name

        glPushMatrix()
        glTranslated(0, 128, 0)
        glScaled(0.55, 0.55, 1)

        GuiDraw.drawRect(0, 0, GuiDraw.getStringWidth(name)+4, 11, 0x5F000000)
        GuiDraw.drawString(name, 2, 2, EnumColour.WHITE.argb, false)

        glPopMatrix()
    }
}

class ItemICChip extends ItemCore
{
    setMaxStackSize(1)
    setHasSubtypes(true)
    setCreativeTab(ProjectRedFabrication.tabFabrication)

    override def addInformation(stack:ItemStack, playerIn:EntityPlayer, tooltip:JList[String], advanced:Boolean)
    {
        ItemICChip.addInfo(stack, tooltip)
        if (stack.getItemDamage == 1)
        {
            tooltip.add("Creative-mode only chip.")
            tooltip.add("Instant and free prints.")
            tooltip.add("Rightclick to add IC Gate to inventory.")
        }
    }

    override def onItemRightClick(stack:ItemStack, world:World, player:EntityPlayer, hand:EnumHand):ActionResult[ItemStack] =
    {
        if (stack.getItemDamage == 1 && ItemICBlueprint.hasICInside(stack)) //creative chip
        {
            val gate = GateDefinition.ICGate.makeStack
            ItemICBlueprint.copyToGate(stack, gate)
            if (!player.inventory.addItemStackToInventory(gate))
                player.entityDropItem(gate, player.getEyeHeight)
            return new ActionResult(EnumActionResult.SUCCESS, stack)
        }
        super.onItemRightClick(stack, world, player, hand)
    }

    override def getSubItems(itemIn:Item, tab:CreativeTabs, subItems:JList[ItemStack])
    {
        subItems.add(new ItemStack(this, 1, 0))
        subItems.add(new ItemStack(this, 1, 1))
    }
}

object ItemICChip
{
    def addInfo(stack:ItemStack, list:JList[String])
    {
        if (ItemICBlueprint.hasICInside(stack))
            list.add(ChatFormatting.GRAY+ItemICBlueprint.getICName(stack))
    }
}