/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.TextureUtils
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.vec.{Point, Rect, Size}
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Slot
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
            for (s <- c.slotCount until c.slotCount+9)
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
            for (s <- c.slotCount until c.slotCount+9)
            {
                val slot = new Slot3(c.chip.asInstanceOf[TChipStock].stock, s, 9999, 9999)
                slot.phantomSlot = true
                c.addSlotToContainer(slot)
            }
        },
        classOf[TChipCrafter] -> {c =>
            for (s <- c.slotCount until c.slotCount+10)
            {
                val slot = new Slot3(c.chip.asInstanceOf[TChipCrafter].matrix, s, 9999, 9999)
                slot.phantomSlot = true
                c.addSlotToContainer(slot)
            }
        }
    )

    val panelFactories = Map[Class[_ <: RoutingChip], RoutingChip => Seq[ChipPanelNode]](
        classOf[TChipFilter] -> { c => Seq(new FilterChipPanel(c.asInstanceOf[TChipFilter])) },
        classOf[TChipOrientation] -> { c => Seq(new OrientChipPanel(c.asInstanceOf[TChipOrientation])) },
        classOf[TChipPriority] -> { c => Seq(new PriorityChipPanel(c.asInstanceOf[TChipPriority])) },
        classOf[TChipStock] -> { c => Seq(new StockChipPanel(c.asInstanceOf[TChipStock])) },
        classOf[TChipCrafter] -> { c => Seq(
            new CraftChipPanel(c.asInstanceOf[TChipCrafter]),
            new CraftExtPanel(c.asInstanceOf[TChipCrafter])
        )}
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
                        val currentlyOpen = children.collect {case c:ChipPanelNode if !c.hidden => c}
                        panel.position = convertPointFromScreen(Point(8, 8)*(currentlyOpen.size+1))
                        panel.unhidePanel()
                        panel.pushZTo(0.1*(currentlyOpen.size+1))
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
        PRResources.guiChipContainer.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)
        TextureUtils.bindAtlas(1)
        drawTexturedModelRectFromIcon(55, 14, c.chip.getChipType.icon, 64, 64)
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        keycode == player.inventory.currentItem+2
    }
}

object GuiChipConfig extends TGuiBuilder
{
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
    override def frame = Rect(position, size)

    var lineColor = Colors.WHITE.argb(0xAA)

    private def moverFrame = Rect(position+Point(4, 9), Size(4, 6))
    private var mouseDown = false
    private var mouseInit = Point.zeroPoint

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

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        if (mouseDown)
        {
            position += mouse-mouseInit
            mouseInit = mouse
        }
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        GuiLib.drawGuiBox(position.x, position.y, size.width, size.height, 0)
        GuiDraw.drawRect(moverFrame.x, moverFrame.y, moverFrame.width, moverFrame.height, Colors.LIGHT_GREY.argb)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        val from = getDotPosition
        val to = from.clamp(frame)
        GL11.glColor4d(1, 1, 1, 1)
        GuiLib.drawLine(from.x, from.y, to.x, to.y, lineColor)
        GuiDraw.drawRect(to.x-3, to.y-3, 6, 6, lineColor)
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        hitTest(p).find(_.isInstanceOf[ChipPanelNode]) match
        {
            case Some(gui) if gui == this =>
                val guis = parent.childrenByZ.collect{case c:ChipPanelNode if !c.hidden => c}
                val otherGuis = guis.filter(_ != this)
                for (i <- otherGuis.indices) otherGuis(i).pushZTo(0.1*(i+1))
                pushZTo(0.1*(otherGuis.size+1))

                if (moverFrame.contains(p))
                {
                    mouseDown = true
                    mouseInit = p
                }
                true
            case _ => false
        }
    }

    override def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        mouseDown = false
        false
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
        if (!consumed && keycode == Keyboard.KEY_ESCAPE)
        {
            hidePanel()
            true
        }
        else false
}