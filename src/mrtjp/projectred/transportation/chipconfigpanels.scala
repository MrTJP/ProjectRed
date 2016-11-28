/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import codechicken.lib.colour.EnumColour
import codechicken.lib.gui.GuiDraw
import codechicken.lib.texture.TextureUtils
import com.mojang.realmsclient.gui.ChatFormatting
import mrtjp.core.gui._
import mrtjp.core.vec.{Point, Size}
import net.minecraft.util.ResourceLocation

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
                TextureUtils.changeTexture(GuiLib.guiExtras)
                drawTexturedModalRect(position.x, position.y, if(chip.filterExclude) 1 else 17, 102, 14, 14)
            }
        }
        b.position = Point(88, 16)
        b.size = Size(14, 14)
        b.tooltipBuilder = { list =>
            list += "Filter mode"
            list += (ChatFormatting.GRAY + "Items are " + (if(chip.filterExclude) "blacklisted" else "whitelisted"))
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
                    TextureUtils.changeTexture(GuiLib.guiExtras)
                    drawTexturedModalRect(position.x, position.y, if (chip.metaMatch) 49 else 65, 118, 14, 14)
                }
            }
            b.position = Point(70, 16)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Metadata matching"
                list += (ChatFormatting.GRAY+"Meta is "+(if (chip.metaMatch) "checked" else "ignored"))
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
                    TextureUtils.changeTexture(GuiLib.guiExtras)
                    drawTexturedModalRect(position.x, position.y, if (chip.nbtMatch) 33 else 49, 102, 14, 14)
                }
            }
            b.position = Point(70, 32)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "NBT matching"
                list += (ChatFormatting.GRAY+"NBT is "+(if (chip.nbtMatch) "checked" else "ignored"))
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
                    TextureUtils.changeTexture(GuiLib.guiExtras)
                    drawTexturedModalRect(position.x, position.y, if (chip.oreMatch) 81 else 97, 118, 14, 14)
                }
            }
            b.position = Point(70, 48)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Ore Dictionary matching"
                list += (ChatFormatting.GRAY+"Ore Dictionary is "+(if (chip.oreMatch) "checked" else "ignored"))
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
                    TextureUtils.changeTexture(GuiLib.guiExtras)
                    val u = chip.damageGroupMode*22+1
                    drawTexturedModalRect(position.x, position.y, u, 80, 20, 20)
                }
            }
            b.position = Point(88, 48)
            b.size = Size(20, 20)
            b.tooltipBuilder = { list =>
                list += "Damage groups"
                val percent = chip.grpPerc(chip.damageGroupMode)
                list += (ChatFormatting.GRAY+(percent match
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
                    TextureUtils.changeTexture(GuiLib.guiExtras)
                    val u = chip.hideMode*16+1
                    drawTexturedModalRect(position.x, position.y, u, 118, 14, 14)
                }
            }
            b.position = Point(88, 32)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Item hiding"
                list += (ChatFormatting.GRAY+"Hide "+(if (chip.hideMode == 0) "nothing" else chip.hide(chip.hideMode)))
            }
            b.clickDelegate = {() =>
                chip.shiftHiding()
                getContainer.saveChip()
            }
            addChild(b)
        }
    }

    override def getDotPosition = Point(85, 34)

    override def getPanelPosition = Point(-60, -16)

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

    override def getPanelPosition = Point(-52, -18)

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
        GuiDraw.drawString("Extraction is", position.x+xOff, position.y+yOff, EnumColour.GRAY.rgb, false)
        if (chip.extractOrient == -1) GuiDraw.drawString("not simulated", position.x+xOff, position.y+yOff+10, EnumColour.GRAY.rgb, false)
        else
        {
            GuiDraw.drawString("simulated from", position.x+xOff, position.y+yOff+10, EnumColour.GRAY.rgb, false)
            GuiDraw.drawString("the " + names(chip.extractOrient), position.x+xOff, position.y+yOff+20, EnumColour.GRAY.rgb, false)
        }
    }
}

class PriorityChipPanel(chip:TChipPriority) extends ChipPanelNode(chip)
{
    size = Size(64, 68)

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
    }

    override def getDotPosition = Point(76, 51)

    override def getPanelPosition = Point(-25, 15)

    override def isPanelVisible = chip.prefScale > 0

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Priority"
        chip.addPriorityInfo(list)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        GuiDraw.drawStringC(chip.preference.toString, position.x+32, position.y+28, EnumColour.GRAY.argb, false)
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
                TextureUtils.changeTexture(GuiLib.guiExtras)
                drawTexturedModalRect(position.x, position.y, 81+16*chip.requestMode, 102, 14, 14)
            }
        }
        b.position = Point(75, 32)
        b.size = Size(14, 14)
        b.tooltipBuilder = {list =>
            list += "Fill mode"
            list += ChatFormatting.GRAY+(chip.requestMode match
            {
                case 0 => "refill when items missing"
                case 1 => "refill when items empty"
                case 2 => "refill infinitely"
            })
        }
        b.clickDelegate = {() =>
            chip.shiftRequestMode()
            getContainer.saveChip()
        }
        addChild(b)
    }

    override def getDotPosition = Point(90, 35)

    override def getPanelPosition = Point(-30, -35)

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

    override def getPanelPosition = Point(-55, -32)

    override def isPanelVisible = true

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Matrix"
        chip.addMatrixInfo(list)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        TextureUtils.changeTexture("textures/gui/container/crafting_table.png")
        GuiDraw.drawTexturedModalRect(position.x+11, position.y+11, 29, 16, 116, 54)
    }
}

class CraftExtPanel(chip:TChipCrafter) extends ChipPanelNode(chip)
{
    size = Size(72, 72)

    override def onAddedToParent_Impl()
    {

        var s = getContainer.indexMap(classOf[TChipCrafter])+10
        for ((x, y) <- GuiLib.createSlotGrid(10, 10, 3, 3, 0, 0))
        {
            val slot = new InventorySlotNode
            slot.position = Point(x, y)
            slot.slotIdx = s
            addChild(slot)
            s += 1
        }
    }

    override def getDotPosition = Point(90, 32)

    override def getPanelPosition = Point(110, -25)

    override def isPanelVisible = true

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Extensions"
        chip.addExtInfo(list)
    }

    override def drawBackgroundBox()
    {
        TextureUtils.changeTexture(CraftExtPanel.backgroundImage)
        GuiDraw.drawTexturedModalRect(position.x, position.y, 0, 0, size.width, size.height)
    }
}

object CraftExtPanel
{
    val backgroundImage = new ResourceLocation("projectred", "textures/gui/craft_chip_ext_panel.png")
}

class MatrixMatchingPanel(chip:TChipMatchMatrix) extends ChipPanelNode(chip)
{
    var idx = -1

    private var idxButtons = Seq.empty[ButtonNode]
    private var settingsButtons = Seq.empty[ButtonNode]

    {
        size = Size(123, 68)

        var b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                TextureUtils.changeTexture(GuiLib.guiExtras)
                GuiDraw.drawTexturedModalRect(position.x, position.y, if (chip.matchMeta(idx)) 49 else 65, 118, 14, 14)
            }
        }
        b.position = Point(70, 12)
        b.size = Size(14, 14)
        b.tooltipBuilder = { list =>
            list += "Metadata matching"
            list += (ChatFormatting.GRAY+"Meta is "+(if (chip.matchMeta(idx)) "checked" else "ignored"))
        }
        b.clickDelegate = {() =>
            chip.toggleMatchMeta(idx)
            getContainer.saveChip()
        }
        addChild(b)
        settingsButtons :+= b

        b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                TextureUtils.changeTexture(GuiLib.guiExtras)
                GuiDraw.drawTexturedModalRect(position.x, position.y, if (chip.matchNBT(idx)) 33 else 49, 102, 14, 14)
            }
        }
        b.position = Point(70, 28)
        b.size = Size(14, 14)
        b.tooltipBuilder = { list =>
            list += "NBT matching"
            list += (ChatFormatting.GRAY+"NBT is "+(if (chip.matchNBT(idx)) "checked" else "ignored"))
        }
        b.clickDelegate = {() =>
            chip.toggleMatchNBT(idx)
            getContainer.saveChip()
        }
        addChild(b)
        settingsButtons :+= b

        b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                TextureUtils.changeTexture(GuiLib.guiExtras)
                GuiDraw.drawTexturedModalRect(position.x, position.y, if (chip.matchOre(idx)) 81 else 97, 118, 14, 14)
            }
        }
        b.position = Point(70, 44)
        b.size = Size(14, 14)
        b.tooltipBuilder = { list =>
            list += "Ore Dictionary matching"
            list += (ChatFormatting.GRAY+"Ore Dictionary is "+(if (chip.matchOre(idx)) "checked" else "ignored"))
        }
        b.clickDelegate = {() =>
            chip.toggleMatchOre(idx)
            getContainer.saveChip()
        }
        addChild(b)
        settingsButtons :+= b

        b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                TextureUtils.changeTexture(GuiLib.guiExtras)
                val u = chip.matchGroup(idx)*22+1
                GuiDraw.drawTexturedModalRect(position.x, position.y, u, 80, 20, 20)
            }
        }
        b.position = Point(88, 26)
        b.size = Size(20, 20)
        b.tooltipBuilder = { list =>
            list += "Damage groups"
            val percent = chip.grpPerc(chip.matchGroup(idx))
            list += (ChatFormatting.GRAY+(percent match
            {
                case -1 => "Tools are not grouped by damage."
                case _ => "Tools grouped at "+percent+"%"
            }))
        }
        b.clickDelegate = {() =>
            chip.toggleMatchGroup(idx)
            getContainer.saveChip()
        }
        addChild(b)
        settingsButtons :+= b

        for (((x, y), i) <- GuiLib.createGrid(10, 10, 3, 3, 18, 18).zipWithIndex)
        {
            val button = new IconButtonNode {
                override def drawButton(mouseover:Boolean) {
                    val stack = chip.getMatchInventory.getStackInSlot(i)
                    if (stack != null)
                        ItemDisplayNode.renderItem(position, size, 0, false, stack)
                }
            }
            button.position = Point(x, y)
            button.size = Size(13, 13)
            button.clickDelegate = {() =>
                idx = i
                idxButtons.foreach(_.mouseoverLock = false)
                button.mouseoverLock = true
                settingsButtons.foreach(_.hidden = false)
            }
            addChild(button)
            idxButtons :+= button
        }

        settingsButtons.foreach(_.hidden = true)
    }

    override def getDotPosition = Point(96, 50)

    override def getPanelPosition = Point(-40, -25)

    override def isPanelVisible = true

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Match options"
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
    }

    override def drawBackgroundBox()
    {
        TextureUtils.changeTexture(CraftExtPanel.backgroundImage)
        GuiDraw.drawTexturedModalRect(position.x, position.y, 0, 72, size.width, size.height)
    }
}

class ExtensionIDPanel(chip:TChipCrafterExtension) extends ChipPanelNode(chip)
{
    size = Size(120, 55)

    {
        val ref = new MCButtonNode
        ref.position = Point(57, 38)
        ref.size = Size(58, 12)
        ref.text = "randomize"
        ref.clickDelegate = {() =>
            chip.randomizeUUID()
            getContainer.saveChip()
        }
        addChild(ref)
    }

    override def getDotPosition = Point(80, 42)

    override def getPanelPosition = Point(-50, -20)

    override def isPanelVisible = true

    override def buildDotTooltip(list:ListBuffer[String])
    {
        list += "Extension ID"
        chip.addExtIDInfo(list)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        val sec = chip.id.toString.split("-")
        val groups = Seq(sec(0)+"-"+sec(1)+"-"+sec(2)+"-"+sec(3)+"-", sec(4))

        val prev = GuiDraw.fontRenderer.getUnicodeFlag
        GuiDraw.fontRenderer.setUnicodeFlag(true)
        for (i <- groups.indices)
            GuiDraw.drawString(groups(i), position.x+14, position.y+12+i*12, EnumColour.GRAY.argb, false)
        GuiDraw.fontRenderer.setUnicodeFlag(prev)
    }
}