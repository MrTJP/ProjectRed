/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.packet.PacketCustom
import codechicken.lib.texture.TextureUtils
import mrtjp.core.gui._
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Slot
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11

import scala.collection.mutable.ListBuffer

class ContainerChipConfig(player:EntityPlayer, var chip:RoutingChip) extends NodeContainer
{
    var slotCount = 0
    var indexMap = Map[Class[_ <: RoutingChip], Int]()

    {
        for ((c, i) <- ContainerChipConfig.panelSlotsMap)
            if (c.isInstance(chip))
            {
                indexMap += c -> slotCount
                i(this)
                slotCount = slots.size
            }

        addPlayerInv(player, 8, 86)
    }

    override def addSlotToContainer(slot:Slot):Slot =
    {
        super.addSlotToContainer(slot)
        if (slot.getSlotIndex == player.inventory.currentItem && slot.inventory == player.inventory)
            slot.asInstanceOf[Slot3].canRemoveDelegate = {() => false}
        slot
    }

    override def onContainerClosed(p:EntityPlayer)
    {
        super.onContainerClosed(p)
        saveChip()
    }

    def saveChip()
    {
        if (player.worldObj.isRemote)
        {
            val stack = player.inventory.getCurrentItem
            ItemRoutingChip.saveChipToItemStack(stack, chip)
            player.inventory.markDirty()

            new PacketCustom(TransportationCPH.channel, TransportationCPH.gui_ChipNBTSet)
                    .writeByte(player.inventory.currentItem).writeItemStack(stack).sendToServer()
        }
    }
}

object ContainerChipConfig
{
    val panelSlotsMap = Map[Class[_ <: RoutingChip], ContainerChipConfig => Unit](
        classOf[TChipFilter] -> {c =>
            for (s <- 0 until 9)
            {
                val slot = new Slot3(c.chip.asInstanceOf[TChipFilter].filter, s, 9999, 9999)
                slot.phantomSlot = true
                c.addSlotToContainer(slot)
            }
        },
        classOf[TChipOrientation] -> {c =>
        },
        classOf[TChipPriority] -> {c =>
        },
        classOf[TChipStock] -> {c =>
            for (s <- 0 until 9)
            {
                val slot = new Slot3(c.chip.asInstanceOf[TChipStock].stock, s, 9999, 9999)
                slot.phantomSlot = true
                c.addSlotToContainer(slot)
            }
        },
        classOf[TChipCrafter] -> {c =>
            for (s <- 0 until 10)
            {
                val slot = new Slot3(c.chip.asInstanceOf[TChipCrafter].matrix, s, 9999, 9999)
                slot.phantomSlot = true
                c.addSlotToContainer(slot)
            }

            for (s <- 0 until 9)
            {
                val slot = new Slot3(c.chip.asInstanceOf[TChipCrafter].extMatrix, s, 9999, 9999)
                slot.phantomSlot = true
                c.addSlotToContainer(slot)
            }
        },
        classOf[TChipCrafterExtension] -> {c =>
        }
    )

    val panelFactories = Map[Class[_ <: RoutingChip], RoutingChip => Seq[ChipPanelNode]](
        classOf[TChipFilter] -> {c => Seq(new FilterChipPanel(c.asInstanceOf[TChipFilter]))},
        classOf[TChipOrientation] -> {c => Seq(new OrientChipPanel(c.asInstanceOf[TChipOrientation]))},
        classOf[TChipPriority] -> {c => Seq(new PriorityChipPanel(c.asInstanceOf[TChipPriority]))},
        classOf[TChipStock] -> {c => Seq(new StockChipPanel(c.asInstanceOf[TChipStock]))},
        classOf[TChipMatchMatrix] -> {c => Seq(new MatrixMatchingPanel(c.asInstanceOf[TChipMatchMatrix]))},
        classOf[TChipCrafter] -> {c => Seq(
            new CraftChipPanel(c.asInstanceOf[TChipCrafter]),
            new CraftExtPanel(c.asInstanceOf[TChipCrafter])
        )},
        classOf[TChipCrafterExtension] -> {c => Seq(new ExtensionIDPanel(c.asInstanceOf[TChipCrafterExtension]))}
    )
}

class GuiChipConfig(player:EntityPlayer, c:ContainerChipConfig) extends NodeGui(c)
{
    var panels = Seq[(TNode, ChipPanelNode)]()
    var isDirty = false

    {
        c.slotChangeDelegate = {_ => isDirty = true}

        for ((cl, factory) <- ContainerChipConfig.panelFactories)
            if (cl.isInstance(c.chip)) factory(c.chip).foreach
            { panel =>
                val dot = new DotSelectNode
                dot.position = panel.getDotPosition-4
                dot.tooltipBuilder = panel.buildDotTooltip
                dot.clickDelegate = {() =>
                    if (panel.hidden)
                    {
                        children.collect {case c:ChipPanelNode => c}.foreach(_.hidePanel())
                        panel.unhidePanel()
                    }
                }
                addChild(dot)
                addChild(panel)
                panels :+= (dot, panel)
            }

        updateDotVisibility()
    }

    def updateDotVisibility()
    {
        for ((dot, panel) <- panels)
            dot.hidden = !panel.isPanelVisible
    }

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        updateDotVisibility()
        if (isDirty)
        {
            c.saveChip()
            isDirty = false
        }
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        TextureUtils.changeTexture(GuiChipConfig.backgroundImage)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)
        TextureUtils.bindBlockTexture()
        drawTexturedModalRect(55, 14, c.chip.getChipType.icon, 64, 64)
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        keycode == player.inventory.currentItem+2
    }
}

object GuiChipConfig extends TGuiFactory
{
    val backgroundImage = new ResourceLocation("projectred", "textures/gui/chip_settings.png")

    override def getID = TransportationProxy.guiIDRoutingChips

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val slot = data.readUByte()
        player.inventory.currentItem = slot
        val stack = player.inventory.getStackInSlot(slot)
        if (ItemRoutingChip.isValidChip(stack))
        {
            val r = ItemRoutingChip.loadChipFromItemStack(stack)
            new GuiChipConfig(player, r.createContainer(player))
        }
        else null
    }
}

abstract class ChipPanelNode(chip:RoutingChip) extends TNode
{
    var size = Size.zeroSize
    position = getPanelPosition
    override def frame = Rect(position, size)

    var lineColor = EnumColour.WHITE.argb(0xAA)

    {
        val close = new MCButtonNode
        close.position = Point(4, 4)
        close.size = Size(5, 5)
        close.clickDelegate = {() => hidePanel()}
        addChild(close)

        hidePanel()
    }

    def getContainer = getRoot.inventorySlots.asInstanceOf[ContainerChipConfig]

    def getDotPosition:Point

    def getPanelPosition:Point

    def isPanelVisible:Boolean

    def buildDotTooltip(list:ListBuffer[String])

    def hidePanel()
    {
        hidden = true
    }

    def unhidePanel()
    {
        hidden = false
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        drawBackgroundBox()
    }

    def drawBackgroundBox()
    {
        GuiLib.drawGuiBox(position.x, position.y, size.width, size.height, 0)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        val from = getDotPosition
        val to = from.clamp(frame)
        GL11.glColor4d(1, 1, 1, 1)
        GuiDraw.drawLine(from.x, from.y, to.x, to.y, 3, lineColor) //TODO this isnt working
        GuiDraw.drawRect(to.x-3, to.y-3, 6, 6, lineColor)
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
        if (!consumed && keycode == Keyboard.KEY_ESCAPE)
        {
            hidePanel()
            true
        }
        else false
}
