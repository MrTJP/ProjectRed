/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.CCRenderState
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.resource.ResourceLib
import mrtjp.core.vec.{Point, Size}
import net.minecraft.client.gui.Gui
import net.minecraft.util.EnumChatFormatting

import scala.collection.mutable.ListBuffer

class FilterChipPanel(chip:TChipFilter) extends ChipPanelNode(chip)
{
    size = Size(115, 75)

    override def onAddedToParent_Impl()
    {

        var s = getContainer.indexMap(classOf[TChipFilter])
        for ((x, y) <- GuiLib.createSlotGrid(12, 12, 3, 3, 0, 0))
        {
            val slot = new InventorySlotNode
            slot.position = Point(x, y)
            slot.slotIdx = s
            addChild(slot)
            s += 1
        }

        val b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                drawTexturedModalRect(position.x, position.y, if(chip.filterExclude) 1 else 17, 102, 14, 14)
            }
        }
        b.position = Point(88, 16)
        b.size = Size(14, 14)
        b.tooltipBuilder = { list =>
            list += "Filter mode"
            list += (EnumChatFormatting.GRAY + "Items are " + (if(chip.filterExclude) "blacklisted" else "whitelisted"))
        }
        b.clickDelegate = {() =>
            chip.toggleExcludeMode()
            getContainer.saveChip()
        }
        addChild(b)

        if (chip.enablePatterns)
        {
            var b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(position.x, position.y, if (chip.metaMatch) 49 else 65, 118, 14, 14)
                }
            }
            b.position = Point(70, 16)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Metadata matching"
                list += (EnumChatFormatting.GRAY+"Meta is "+(if (chip.metaMatch) "checked" else "ignored"))
            }
            b.clickDelegate = {() =>
                chip.toggleMetaMode()
                getContainer.saveChip()
            }
            addChild(b)

            b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(position.x, position.y, if (chip.nbtMatch) 33 else 49, 102, 14, 14)
                }
            }
            b.position = Point(70, 32)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "NBT matching"
                list += (EnumChatFormatting.GRAY+"NBT is "+(if (chip.nbtMatch) "checked" else "ignored"))
            }
            b.clickDelegate = {() =>
                chip.toggleNBTMode()
                getContainer.saveChip()
            }
            addChild(b)

            b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(position.x, position.y, if (chip.oreMatch) 81 else 97, 118, 14, 14)
                }
            }
            b.position = Point(70, 48)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Ore Dictionary matching"
                list += (EnumChatFormatting.GRAY+"Ore Dictionary is "+(if (chip.oreMatch) "checked" else "ignored"))
            }
            b.clickDelegate = {() =>
                chip.toggleOreMode()
                getContainer.saveChip()
            }
            addChild(b)

            b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    val u = chip.damageGroupMode*22+1
                    drawTexturedModalRect(position.x, position.y, u, 80, 20, 20)
                }
            }
            b.position = Point(88, 48)
            b.size = Size(20, 20)
            b.tooltipBuilder = { list =>
                list += "Damage groups"
                val percent = chip.grpPerc(chip.damageGroupMode)
                list += (EnumChatFormatting.GRAY+(percent match
                {
                    case -1 => "Tools are not grouped by damage."
                    case _ => "Tools grouped at "+percent+"%"
                }))
            }
            b.clickDelegate = {() =>
                chip.shiftDamageGroup()
                getContainer.saveChip()
            }
            addChild(b)
        }

        if (chip.enableHiding)
        {
            val b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    val u = chip.hideMode*16+1
                    drawTexturedModalRect(position.x, position.y, u, 118, 14, 14)
                }
            }
            b.position = Point(88, 32)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Item hiding"
                list += (EnumChatFormatting.GRAY+"Hide "+(if (chip.hideMode == 0) "nothing" else chip.hide(chip.hideMode)))
            }
            b.clickDelegate = {() =>
                chip.shiftHiding()
                getContainer.saveChip()
            }
            addChild(b)
        }
    }

    override def getDotPosition = Point(85, 34)

    override def isPanelVisible = chip.enableFilter

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Filter"
        chip.addFilterInfo(list)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        for ((x, y) <- GuiLib.createSlotGrid(position.x+12, position.y+12, 3, 3, 0, 0))
            GuiLib.drawSlotBackground(x-1, y-1)
    }
}

class OrientChipPanel(chip:TChipOrientation) extends ChipPanelNode(chip)
{
    {
        size = Size(130, 58)

        val sideWidget = new SideSelectNode(12, 12, 30, 30)
        {
            override def onSideChanged(oldside:Int)
            {
                chip.extractOrient = if (sides != 0) Integer.numberOfTrailingZeros(sides) else -1
                getContainer.saveChip()
            }
        }
        sideWidget.exclusiveSides = true
        if (chip.extractOrient >= 0) sideWidget.sides = 1<<chip.extractOrient
        addChild(sideWidget)
    }

    override def getDotPosition = Point(100, 50)

    override def isPanelVisible = true

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Orientation"
        chip.addOrientInfo(list)
    }

    private val names = Seq("bottom", "top", "North", "South", "West", "East")

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        val xOff = 52
        val yOff = 16
        GuiDraw.drawString("Extraction is", position.x+xOff, position.y+yOff, Colors.GREY.rgb, false)
        if (chip.extractOrient == -1) GuiDraw.drawString("not simulated", position.x+xOff, position.y+yOff+10, Colors.GREY.rgb, false)
        else
        {
            GuiDraw.drawString("simulated from", position.x+xOff, position.y+yOff+10, Colors.GREY.rgb, false)
            GuiDraw.drawString("the " + names(chip.extractOrient), position.x+xOff, position.y+yOff+20, Colors.GREY.rgb, false)
        }
    }
}

class PriorityChipPanel(chip:TChipPriority) extends ChipPanelNode(chip)
{
    size = Size(64, 72)

    {
        val plus = new MCButtonNode
        plus.position = Point(26, 12)
        plus.size = Size(12, 12)
        plus.text = "+"
        plus.clickDelegate = {() =>
            chip.prefUp()
            getContainer.saveChip()
        }
        addChild(plus)

        val minus = new MCButtonNode
        minus.position = Point(26, 40)
        minus.size = Size(12, 12)
        minus.text = "-"
        minus.clickDelegate = {() =>
            chip.prefDown()
            getContainer.saveChip()
        }
        addChild(minus)

        if (chip.enablePriorityFlag)
        {
            val check = CheckBoxNode.centered(52, 60)
            check.state = chip.priorityFlag
            check.clickDelegate = {() => chip.priorityFlag = check.state}
            addChild(check)
        }

    }

    override def getDotPosition = Point(76, 51)

    override def isPanelVisible = chip.prefScale > 0

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Priority"
        chip.addPriorityInfo(list)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        GuiDraw.drawStringC(chip.preference.toString, position.x+32, position.y+28, Colors.GREY.argb, false)
        if (chip.enablePriorityFlag) GuiDraw.drawString("Enabled", position.x+4, position.y+56, Colors.GREY.rgb, false)
    }
}

class StockChipPanel(chip:TChipStock) extends ChipPanelNode(chip)
{
    override def onAddedToParent_Impl()
    {
        size = Size(105, 75)

        var s = getContainer.indexMap(classOf[TChipStock])
        for ((x, y) <- GuiLib.createSlotGrid(12, 12, 3, 3, 0, 0))
        {
            val slot = new InventorySlotNode
            slot.position = Point(x, y)
            slot.slotIdx = s
            addChild(slot)
            s += 1
        }

        val b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                drawTexturedModalRect(position.x, position.y, if (chip.requestWhenEmpty) 97 else 81, 102, 14, 14)
            }
        }
        b.position = Point(75, 32)
        b.size = Size(14, 14)
        b.tooltipBuilder = {list =>
            list += "Fill mode"
            list += (EnumChatFormatting.GRAY+"refill when items "+(if (chip.requestWhenEmpty) "empty" else "missing"))
        }
        b.clickDelegate = {() =>
            chip.shiftRequestMode()
            getContainer.saveChip()
        }
        addChild(b)
    }

    override def getDotPosition = Point(90, 50)

    override def isPanelVisible = true

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Stock"
        chip.addStockInfo(list)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        for ((x, y) <- GuiLib.createSlotGrid(position.x+12, position.y+12, 3, 3, 0, 0))
            GuiLib.drawSlotBackground(x-1, y-1)
    }
}

class CraftChipPanel(chip:TChipCrafter) extends ChipPanelNode(chip)
{
    size = Size(140, 78)

    override def onAddedToParent_Impl()
    {
        var s = getContainer.indexMap(classOf[TChipCrafter])
        for ((x, y) <- GuiLib.createSlotGrid(12, 12, 3, 3, 0, 0))
        {
            val slot = new InventorySlotNode
            slot.position = Point(x, y)
            slot.slotIdx = s
            addChild(slot)
            s += 1
        }

        val slot = new InventorySlotNode
        slot.position = Point(12+94, 12+18)
        slot.slotIdx = s
        addChild(slot)
    }

    override def getDotPosition = Point(105, 42)

    override def isPanelVisible = true

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Matrix"
        chip.addMatrixInfo(list)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        CCRenderState.changeTexture("textures/gui/container/crafting_table.png")
        GuiDraw.drawTexturedModalRect(position.x+11, position.y+11, 29, 16, 116, 54)
    }
}

class CraftExtPanel(chip:TChipCrafter) extends ChipPanelNode(chip)
{
    {
        size = Size(120, 86)

        import scala.util.control.Breaks._
        breakable { for (((x, y), i) <- GuiLib.createGrid(size.width/2-40, 6, 3, 3, 24+4, 24+1).zipWithIndex)
            if (chip.maxExtensions >= i)
            {
                val up = new MCButtonNode
                up.position = Point(x, y)
                up.size = Size(24, 6)
                up.clickDelegate = {() =>
                    chip.extUp(i)
                    getContainer.saveChip()
                }
                addChild(up)

                val down = new MCButtonNode
                down.position = Point(x, y+18)
                down.size = Size(24, 6)
                down.clickDelegate = {() =>
                    chip.extDown(i)
                    getContainer.saveChip()
                }
                addChild(down)
            }
            else break()
        }
    }

    override def getDotPosition = Point(90, 32)

    override def isPanelVisible = chip.maxExtensions > 0

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Extensions"
        chip.addExtInfo(list)
    }


    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)

        var index = 0
        for ((x, y) <- GuiLib.createGrid(position.x+size.width/2-40, position.y+6, 3, 3, 28, 25))
        {
            if (chip.maxExtensions >= index)
            {
                val ext = chip.extIndex(index)
                if (ext >= 0)
                    Gui.drawRect(x+2, y, x+2+20, y+18, Colors(ext).argb)
                else GuiDraw.drawStringC("off", x+12, y+8, Colors.GREY.rgba, false)
            }
            else GuiDraw.drawStringC("-", x+12, y+8, Colors.GREY.rgba, false)
            index += 1
        }
    }
}