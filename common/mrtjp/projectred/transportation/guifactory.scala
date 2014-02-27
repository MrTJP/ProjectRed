package mrtjp.projectred.transportation

import codechicken.lib.render.{FontUtils, CCRenderState}
import mrtjp.projectred.core.inventory._
import mrtjp.projectred.core.utils.Pair2
import mrtjp.projectred.core.{PRColors, BasicGuiUtils}
import mrtjp.projectred.transportation.RoutingChipContainerFactory.ChipContainer
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.{Gui, GuiScreen}
import net.minecraft.client.renderer.texture.TextureMap
import net.minecraft.tileentity.TileEntityChest
import net.minecraft.util.{EnumChatFormatting, ResourceLocation}
import org.lwjgl.opengl.GL11
import scala.collection.mutable.ListBuffer
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended

object RoutingChipGuiFactory
{
    def apply(c:ChipContainer[RoutingChipset]) = new GuiChipRoot(c)
}

object GuiChipContainerWidget
{
    val resource = new ResourceLocation("projectred:textures/gui/chipcontainer.png")
}

abstract class GuiChipContainer[T <: RoutingChipset](cont:ChipContainer[T], prev:GuiScreen) extends GhostGuiContainer(cont, prev)
{
    def chip = cont.getChip
    def cleanContainer = cont.getNewInstance

    override def keyTyped(par1:Char, id:Int)
    {
        if (id >= 2 && id <= 10)
        {
            val actualKeyboardButton:Int = id - 1
            if (actualKeyboardButton == Minecraft.getMinecraft.thePlayer.inventory.currentItem + 1) return
        }
        else super.keyTyped(par1, id)
    }

    override def drawBackground()
    {
        GL11.glColor4f(1, 1, 1, 1)
        drawChipContainerBackground()
        drawChipIcon()
        if (getPreviousScreen != null) drawChipOverlay()
    }

    /** Utils **/
    def drawChipContainerBackground()
    {
        CCRenderState.changeTexture(GuiChipContainerWidget.resource)
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
    }

    def drawChipIcon()
    {
        CCRenderState.changeTexture(TextureMap.locationItemsTexture)
        drawTexturedModelRectFromIcon(55, 14, chip.getChipType.icon, 64, 64)
    }

    def drawChipOverlay()
    {
        drawGradientRect(5, 5, 171, 80, -1072689136, -804253680)
    }
}

class GuiChipRoot(cont:ChipContainer[RoutingChipset]) extends GuiChipContainer[RoutingChipset](cont, null)
{

    override def actionPerformed(ident:String)
    {
        val c = cleanContainer
        c.addPlayerInventory(8, 86)

        def cFor[T<:RoutingChipset] = c.asInstanceOf[ChipContainer[T]]

        ident match
        {
            case "filt" if chip.isInstanceOf[TChipFilter] =>
                val c2 = chip.asInstanceOf[TChipFilter]
                var s = 0
                import scala.collection.JavaConversions._
                for (p <- BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                {
                    c.addCustomSlot(new SlotExtended(c2.filter, s, p.getValue1, p.getValue2).setGhosting(true))
                    s += 1
                }
                shiftScreen(new GuiChipFilter(cFor[TChipFilter], this), true)

            case "orient" if chip.isInstanceOf[TChipOrientation] => shiftScreen(new GuiChipOrient(cFor[TChipOrientation], this), true)
            case "prior" if chip.isInstanceOf[TChipPriority] => shiftScreen(new GuiChipPriority(cFor[TChipPriority], this), true)
            case "stock" if chip.isInstanceOf[TChipStock] =>
                val c2 = chip.asInstanceOf[TChipStock]
                var s = 0
                import scala.collection.JavaConversions._
                for (p <- BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                {
                    c.addCustomSlot(new SlotExtended(c2.stock, s, p.getValue1, p.getValue2).setGhosting(true))
                    s += 1
                }
                shiftScreen(new GuiChipStock(cFor[TChipStock], this), true)
            case "craftmatrix" if chip.isInstanceOf[TChipCrafter] =>
                val c2 = chip.asInstanceOf[TChipCrafter]
                var s = 0
                import scala.collection.JavaConversions._
                for (p <- BasicGuiUtils.createSlotArray(25, 15, 3, 3, 0, 0))
                {
                    c.addCustomSlot(new SlotExtended(c2.matrix, s, p.getValue1, p.getValue2).setGhosting(true))
                    s += 1
                }
                c.addCustomSlot(new SlotExtended(c2.matrix, s, 119, 33).setGhosting(true))
                shiftScreen(new GuiChipCraftMatrix(cFor[TChipCrafter], this), true)
            case "craftext" if chip.isInstanceOf[TChipCrafter] => shiftScreen(new GuiChipCraftExt(cFor[TChipCrafter], this), true)

        }
    }
    override def addWidgets()
    {
        if (chip.isInstanceOf[TChipFilter] && chip.asInstanceOf[TChipFilter].enableFilter)
            add(new WidgetDotSelector(85, 34)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Filter"
                    chip.asInstanceOf[TChipFilter].addFilterInfo(list)
                }
            }.setActionCommand("filt"))

        if (chip.isInstanceOf[TChipOrientation])
            add(new WidgetDotSelector(100, 50)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Orientation"
                    chip.asInstanceOf[TChipOrientation].addOrientInfo(list)
                }
            }.setActionCommand("orient"))

        if (chip.isInstanceOf[TChipPriority] && chip.asInstanceOf[TChipPriority].prefScale > 0)
            add(new WidgetDotSelector(76, 51)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Priority"
                    chip.asInstanceOf[TChipPriority].addPriorityInfo(list)
                }
            }.setActionCommand("prior"))

        if (chip.isInstanceOf[TChipStock])
            add(new WidgetDotSelector(90, 50)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Stock"
                    chip.asInstanceOf[TChipStock].addStockInfo(list)
                }
            }.setActionCommand("stock"))
        if (chip.isInstanceOf[TChipCrafter])
        {
            val chip2 = chip.asInstanceOf[TChipCrafter]
            add(new WidgetDotSelector(105, 42)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Matrix"
                    chip2.addMatrixInfo(list)
                }
            }.setActionCommand("craftmatrix"))

            if (chip2.maxExtensions>0) add(new WidgetDotSelector(90, 32)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Extensions"
                    chip2.addExtInfo(list)
                }
            }.setActionCommand("craftext"))
        }
    }
}

class GuiChipFilter(cont:ChipContainer[TChipFilter], prev:GuiScreen) extends GuiChipContainer[TChipFilter](cont, prev)
{
    override def drawBackground()
    {
        super.drawBackground()

        import scala.collection.JavaConversions._
        if (chip.enableFilter)
            for (p <- BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1 - 1, p.getValue2 - 1)
    }

    override def addWidgets()
    {
        if (chip.enableFilter) add(new WidgetButton(130, 16, 14, 14) with TButtonMCStyle
        {
            override def drawButton(mouseover:Boolean)
            {
                CCRenderState.changeTexture(GhostWidget.guiExtras)
                drawTexturedModalRect(x, y, if (chip.filterExclude) 1 else 17, 102, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+="Filter mode"
                list+=(EnumChatFormatting.GRAY+"Items are "+(if (chip.filterExclude) "blacklisted" else "whitelisted"))
            }
        }.setActionCommand("filtmode"))

        if (chip.enablePatterns)
        {
            add(new WidgetButton(150, 16, 14, 14) with TButtonMCStyle
            {
                override def drawButton(mouseover:Boolean)
                {
                    CCRenderState.changeTexture(GhostWidget.guiExtras)
                    drawTexturedModalRect(x, y, if (chip.metaMatch) 49 else 65, 118, 14, 14)
                }

                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Metadata matching"
                    list+=(EnumChatFormatting.GRAY+"Meta is "+(if (chip.metaMatch) "checked" else "ignored"))
                }
            }.setActionCommand("md"))

            add(new WidgetButton(150, 32, 14, 14) with TButtonMCStyle
            {
                override def drawButton(mouseover:Boolean)
                {
                    CCRenderState.changeTexture(GhostWidget.guiExtras)
                    drawTexturedModalRect(x, y, if (chip.nbtMatch) 33 else 49, 102, 14, 14)
                }

                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="NBT matching"
                    list+=(EnumChatFormatting.GRAY+"NBT is "+(if (chip.nbtMatch) "checked" else "ignored"))
                }
            }.setActionCommand("nbt"))

            add(new WidgetButton(150, 48, 14, 14) with TButtonMCStyle
            {
                override def drawButton(mouseover:Boolean)
                {
                    CCRenderState.changeTexture(GhostWidget.guiExtras)
                    drawTexturedModalRect(x, y, if (chip.oreMatch) 81 else 97, 118, 14, 14)
                }

                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Ore Dictionary matching"
                    list+=(EnumChatFormatting.GRAY+"Ore Dictionary is "+(if (chip.oreMatch) "checked" else "ignored"))
                }
            }.setActionCommand("ore"))

            add(new WidgetButton(125, 35, 20, 20) with TButtonMCStyle
            {
                override def drawButton(mouseover:Boolean)
                {
                    CCRenderState.changeTexture(GhostWidget.guiExtras)
                    val u = chip.damageGroupMode*22+1
                    drawTexturedModalRect(x, y, u, 80, 20, 20)
                }
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Damage groups"
                    list+=(EnumChatFormatting.GRAY+"Tools grouped at "+chip.grpPerc(chip.damageGroupMode)+"%")
                }
            }.setActionCommand("grp"))
        }

        if (chip.enableHiding) add(new WidgetButton(114, 16, 14, 14) with TButtonMCStyle
        {
            override def drawButton(mouseover:Boolean)
            {
                CCRenderState.changeTexture(GhostWidget.guiExtras)
                val u = chip.hideMode*16+1
                drawTexturedModalRect(x, y, u, 118, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+="Item hiding"
                list+=(EnumChatFormatting.GRAY+"Hide "+(if (chip.hideMode == 0) "nothing" else chip.hide(chip.hideMode)))
            }
        }.setActionCommand("hide"))
    }

    override def actionPerformed(ident:String) = ident match
    {
        case "filtmode" => chip.toggleExcludeMode()
        case "md" => chip.toggleMetaMode()
        case "nbt" => chip.toggleNBTMode()
        case "ore" => chip.toggleOreMode()
        case "grp" => chip.shiftDamageGroup()
        case "hide" => chip.shiftHiding()
    }
}

class GuiChipOrient(cont:ChipContainer[TChipOrientation], prev:GuiScreen) extends GuiChipContainer[TChipOrientation](cont, prev)
{
    val sideWidget = new WidgetSideSelect(20 ,20, 50, 50, 40) with TWidgetSideTE with TWidgetSideHighlight with TWidgetSidePicker
    {
        override def onSideChanged(s:Int) = shiftSides()
    }

    sideWidget.setSideHighlighting(true)
    sideWidget.setTile(new TileEntityChest)
    sideWidget.setExclusive(true)
    if (chip.extractOrient >= 0) sideWidget.setSideMask(1<<chip.extractOrient)

    private val names = Seq[String]("bottom", "top", "North", "South", "West", "East")

    override def drawBackground()
    {
        super.drawBackground()

        val xOff = 90
        fontRenderer.drawString("Extraction is", xOff, 20, PRColors.WHITE.rgb, true)
        if (chip.extractOrient == -1) fontRenderer.drawString("not simulated", xOff, 30, PRColors.WHITE.rgb, true)
        else
        {
            fontRenderer.drawString("simulated from", xOff, 30, PRColors.WHITE.rgb, true)
            fontRenderer.drawString("the " + names(chip.extractOrient), xOff, 40, PRColors.WHITE.rgb, true)
        }
    }

    def shiftSides()
    {
        val s = sideWidget.sideMask
        for (i <- 0 until 6)
            if ((s&1<<i) != 0)
            {
                chip.extractOrient = i
                return
            }

        chip.extractOrient = -1
    }

    override def addWidgets()
    {
        add(sideWidget)
    }
}

class GuiChipPriority(cont:ChipContainer[TChipPriority], prev:GuiScreen) extends GuiChipContainer[TChipPriority](cont, prev)
{
    override def drawBackground()
    {
        super.drawBackground()

        FontUtils.drawCenteredString(chip.preference.toString, 88, 38, PRColors.WHITE.rgb)
        if (chip.enablePriorityFlag) fontRenderer.drawStringWithShadow("Enabled", 98, 68, PRColors.WHITE.rgb)
    }

    override def addWidgets()
    {
        add((new WidgetButton(82, 22, 12, 12) with TButtonMCStyle with TButtonTextOverlay).setText("+").setActionCommand("u"))
        add((new WidgetButton(82, 50, 12, 12) with TButtonMCStyle with TButtonTextOverlay).setText("-").setActionCommand("d"))
        if (chip.enablePriorityFlag) add(new WidgetCheckBox(88, 72, chip.priorityFlag)
        {
            override def onStateChanged(oldState:Boolean)
            {
                chip.priorityFlag = getChecked
            }
        })
    }

    override def actionPerformed(ident:String) = ident match
    {
        case "u" => chip.prefUp()
        case "d" => chip.prefDown()
    }
}

class GuiChipStock(cont:ChipContainer[TChipStock], prev:GuiScreen) extends GuiChipContainer[TChipStock](cont, prev)
{
    override def drawBackground()
    {
        super.drawBackground()
        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
            BasicGuiUtils.drawSlotBackground(mc, p.getValue1 - 1, p.getValue2 - 1)
    }

    override def addWidgets()
    {
        add(new WidgetButton(150, 16, 14, 14) with TButtonMCStyle
        {
            override def drawButton(mouseover:Boolean)
            {
                CCRenderState.changeTexture(GhostWidget.guiExtras)
                drawTexturedModalRect(x, y, if (chip.requestWhenEmpty) 97 else 81, 102, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+="Fill mode"
                list+=(EnumChatFormatting.GRAY+"refill when items "+(if (chip.requestWhenEmpty) "empty" else "missing"))
            }
        }.setActionCommand("fillmode"))
    }

    override def actionPerformed(ident:String) = ident match
    {
        case "fillmode" => chip.shiftRequestMode()
    }
}

class GuiChipCraftMatrix(cont:ChipContainer[TChipCrafter], prev:GuiScreen) extends GuiChipContainer[TChipCrafter](cont, prev)
{
    val tableResource = new ResourceLocation("textures/gui/container/crafting_table.png")

    override def drawBackground()
    {
        super.drawBackground()

        CCRenderState.changeTexture(tableResource)
        drawTexturedModalRect(15, 10, 20, 12, 146, 62)
    }
}

class GuiChipCraftExt(cont:ChipContainer[TChipCrafter], prev:GuiScreen) extends GuiChipContainer[TChipCrafter](cont, prev)
{
    override def drawBackground()
    {
        super.drawBackground()
        var index:Int = 0
        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createGridArray(xSize/2-40, 6, 3, 3, 24+4, 24+1))
        {
            if (chip.maxExtensions >= index)
            {
                val ext = chip.extIndex(index)
                if (ext >= 0)
                {
                    val x = p.getValue1+2
                    val y = p.getValue2+6
                    Gui.drawRect(x, y, x+20, y+18, PRColors.get(ext).argb)
                }
                else drawCenteredString(fontRenderer, "off", p.getValue1+12, p.getValue2+8, PRColors.WHITE.rgba)
            }
            else drawCenteredString(fontRenderer, "-", p.getValue1+12, p.getValue2+8, PRColors.GREY.rgba)
            index += 1
        }
    }

    override def addWidgets()
    {
        var index = 0
        import mrtjp.projectred.core.utils.LabelBreaks._
        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createGridArray(xSize/2-40, 6, 3, 3, 24+4, 24+1)) label("1")
        {
            if (chip.maxExtensions >= index)
            {
                add((new WidgetButton(p.getValue1, p.getValue2, 24, 6) with TButtonMCStyle with TButtonTextOverlay).setActionCommand(index+"u"))
                add((new WidgetButton(p.getValue1, p.getValue2+18, 24, 6) with TButtonMCStyle with TButtonTextOverlay).setActionCommand(index+"d"))
            }
            else break("1")
            index += 1
        }
    }

    override def actionPerformed(ident:String)
    {
        val index = Integer.parseInt(ident.substring(0, 1))

        if (ident.substring(1) == "u") chip.extUp(index)
        else chip.extDown(index)
    }
}