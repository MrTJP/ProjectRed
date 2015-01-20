package mrtjp.projectred.transportation

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.{CCRenderState, FontUtils, TextureUtils}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors_old
import mrtjp.core.gui._
import mrtjp.core.resource.ResourceLib
import mrtjp.core.vec.Point
import mrtjp.projectred.core.libmc.PRResources
import mrtjp.projectred.core.libmc.gui._
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal
import net.minecraft.client.gui.{Gui, GuiScreen}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Slot
import net.minecraft.tileentity.TileEntityChest
import net.minecraft.util.{EnumChatFormatting, ResourceLocation}
import org.lwjgl.opengl.GL11

import scala.collection.mutable.ListBuffer

object ChipGuiFactory extends TGuiBuilder
{
    override def getID = TransportationProxy.guiIDRoutingChips

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val slot = data.readUByte()
        player.inventory.currentItem = slot
        val stack = player.inventory.getStackInSlot(slot)
        RoutingChipDefs.getForStack(stack) match
        {
            case e:ChipVal => ChipGuiFactory(e.createChipset.createContainer(player))
            case _ => null
        }
    }

    @SideOnly(Side.CLIENT)
    def apply(c:ChipContainer) = new GuiChipRoot(c)
}

class ChipContainer(player:EntityPlayer, var chip:RoutingChip) extends WidgetContainer
{
    def this(player:EntityPlayer) = this(player, ItemRoutingChip.loadChipFromItemStack(player.getHeldItem))

    private val slot = player.inventory.currentItem
    private val stack = player.inventory.mainInventory(slot)

    def getNewInstance = new ChipContainer(player, chip)

    override def onContainerClosed(player:EntityPlayer)
    {
        super.onContainerClosed(player)
        if (player.worldObj.isRemote)
        {
            ItemRoutingChip.saveChipToItemStack(player.inventory.mainInventory(slot), chip)
            player.inventory.markDirty()
            new PacketCustom(TransportationCPH.channel, TransportationCPH.gui_ChipNBTSet)
                .writeByte(slot).writeItemStack(player.inventory.mainInventory(slot)).sendToServer()
        }
    }

    override def addSlotToContainer(slot:Slot):Slot =
    {
        if (slot.getSlotIndex == this.slot && slot.inventory == player.inventory) if (slot.isInstanceOf[Slot2])
            return super.addSlotToContainer(slot.asInstanceOf[Slot2].setRemove(false))
        super.addSlotToContainer(slot)
    }
}

abstract class GuiChipContainer[T <: RoutingChip](cont:ChipContainer, prev:GuiScreen) extends WidgetGui(cont)
{
    setJumpBack(prev)

    def chip:T = cont.chip.asInstanceOf[T]
    def cleanContainer = cont.getNewInstance

    override def blockedHotkeyNumbers = Set(mcInst.thePlayer.inventory.currentItem+1)

    override final def drawBack_Impl(mouse:Point, frame:Float)
    {
        GL11.glColor4f(1, 1, 1, 1)
        drawChipContainerBackground()
        drawChipIcon()
        if (prevGui != null) drawChipOverlay()
        drawBackExtra(mouse, frame)
    }

    def drawBackExtra(mouse:Point, frame:Float){}

    /** Utils **/
    def drawChipContainerBackground()
    {
        PRResources.guiChipContainer.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
    }

    def drawChipIcon()
    {
        TextureUtils.bindAtlas(1)
        drawTexturedModelRectFromIcon(55, 14, chip.getChipType.icon, 64, 64)
    }

    def drawChipOverlay()
    {
        drawGradientRect(5, 5, 171, 80, -1072689136, -804253680)
    }
}

class GuiChipRoot(cont:ChipContainer) extends GuiChipContainer[RoutingChip](cont, null)
{
    override def receiveMessage_Impl(message:String)
    {
        val c = cleanContainer
        c.addPlayerInv(8, 86)

        message match
        {
            case "filt" if chip.isInstanceOf[TChipFilter] =>
                val c2 = chip.asInstanceOf[TChipFilter]
                var s = 0
                for ((x, y) <- GuiLib.createSlotGrid(20, 15, 3, 3, 0, 0))
                {
                    c + new Slot2(c2.filter, s, x, y).setGhosting(true)
                    s += 1
                }
                jumpTo(new GuiChipFilter(c, this), true)

            case "orient" if chip.isInstanceOf[TChipOrientation] => jumpTo(new GuiChipOrient(c, this), true)
            case "prior" if chip.isInstanceOf[TChipPriority] => jumpTo(new GuiChipPriority(c, this), true)
            case "stock" if chip.isInstanceOf[TChipStock] =>
                val c2 = chip.asInstanceOf[TChipStock]
                var s = 0
                for ((x, y) <- GuiLib.createSlotGrid(20, 15, 3, 3, 0, 0))
                {
                    c + new Slot2(c2.stock, s, x, y).setGhosting(true)
                    s += 1
                }
                jumpTo(new GuiChipStock(c, this), true)
            case "craftmatrix" if chip.isInstanceOf[TChipCrafter] =>
                val c2 = chip.asInstanceOf[TChipCrafter]
                var s = 0
                for ((x, y) <- GuiLib.createSlotGrid(25, 15, 3, 3, 0, 0))
                {
                    c + new Slot2(c2.matrix, s, x, y).setGhosting(true)
                    s += 1
                }
                c + new Slot2(c2.matrix, s, 119, 33).setGhosting(true)
                jumpTo(new GuiChipCraftMatrix(c, this), true)
            case "craftext" if chip.isInstanceOf[TChipCrafter] => jumpTo(new GuiChipCraftExt(c, this), true)

        }
    }
    override def runInit_Impl()
    {
        if (chip.isInstanceOf[TChipFilter] && chip.asInstanceOf[TChipFilter].enableFilter)
            add(new WidgetDotSelect(85, 34)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Filter"
                    chip.asInstanceOf[TChipFilter].addFilterInfo(list)
                }
            }.setAction("filt"))

        if (chip.isInstanceOf[TChipOrientation])
            add(new WidgetDotSelect(100, 50)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Orientation"
                    chip.asInstanceOf[TChipOrientation].addOrientInfo(list)
                }
            }.setAction("orient"))

        if (chip.isInstanceOf[TChipPriority] && chip.asInstanceOf[TChipPriority].prefScale > 0)
            add(new WidgetDotSelect(76, 51)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Priority"
                    chip.asInstanceOf[TChipPriority].addPriorityInfo(list)
                }
            }.setAction("prior"))

        if (chip.isInstanceOf[TChipStock])
            add(new WidgetDotSelect(90, 50)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Stock"
                    chip.asInstanceOf[TChipStock].addStockInfo(list)
                }
            }.setAction("stock"))
        if (chip.isInstanceOf[TChipCrafter])
        {
            val chip2 = chip.asInstanceOf[TChipCrafter]
            add(new WidgetDotSelect(105, 42)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Matrix"
                    chip2.addMatrixInfo(list)
                }
            }.setAction("craftmatrix"))

            if (chip2.maxExtensions>0) add(new WidgetDotSelect(90, 32)
            {
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Extensions"
                    chip2.addExtInfo(list)
                }
            }.setAction("craftext"))
        }
    }
}

class GuiChipFilter(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipFilter](cont, prev)
{

    override def drawBackExtra(mouse:Point, frame:Float)
    {
        if (chip.enableFilter)
            for ((x, y) <- GuiLib.createSlotGrid(20, 15, 3, 3, 0, 0))
                GuiLib.drawSlotBackground(x-1, y-1)
    }

    override def runInit_Impl()
    {
        if (chip.enableFilter) add(new WidgetButtonIcon(130, 16, 14, 14)
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                drawTexturedModalRect(x, y, if (chip.filterExclude) 1 else 17, 102, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+="Filter mode"
                list+=(EnumChatFormatting.GRAY+"Items are "+(if (chip.filterExclude) "blacklisted" else "whitelisted"))
            }
        }.setAction("filtmode"))

        if (chip.enablePatterns)
        {
            add(new WidgetButtonIcon(150, 16, 14, 14)
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(x, y, if (chip.metaMatch) 49 else 65, 118, 14, 14)
                }

                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Metadata matching"
                    list+=(EnumChatFormatting.GRAY+"Meta is "+(if (chip.metaMatch) "checked" else "ignored"))
                }
            }.setAction("md"))

            add(new WidgetButtonIcon(150, 32, 14, 14)
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(x, y, if (chip.nbtMatch) 33 else 49, 102, 14, 14)
                }

                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="NBT matching"
                    list+=(EnumChatFormatting.GRAY+"NBT is "+(if (chip.nbtMatch) "checked" else "ignored"))
                }
            }.setAction("nbt"))

            add(new WidgetButtonIcon(150, 48, 14, 14)
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(x, y, if (chip.oreMatch) 81 else 97, 118, 14, 14)
                }

                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Ore Dictionary matching"
                    list+=(EnumChatFormatting.GRAY+"Ore Dictionary is "+(if (chip.oreMatch) "checked" else "ignored"))
                }
            }.setAction("ore"))

            add(new WidgetButtonIcon(125, 35, 20, 20)
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    val u = chip.damageGroupMode*22+1
                    drawTexturedModalRect(x, y, u, 80, 20, 20)
                }
                override def buildTooltip(list:ListBuffer[String])
                {
                    list+="Damage groups"
                    val percent = chip.grpPerc(chip.damageGroupMode)
                    list+=(EnumChatFormatting.GRAY+(percent match
                    {
                        case -1 => "Tools are not grouped by damage."
                        case _ => "Tools grouped at "+percent+"%"
                    }))
                }
            }.setAction("grp"))
        }

        if (chip.enableHiding) add(new WidgetButtonIcon(114, 16, 14, 14)
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                val u = chip.hideMode*16+1
                drawTexturedModalRect(x, y, u, 118, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+="Item hiding"
                list+=(EnumChatFormatting.GRAY+"Hide "+(if (chip.hideMode == 0) "nothing" else chip.hide(chip.hideMode)))
            }
        }.setAction("hide"))
    }

    override def receiveMessage_Impl(message:String) = message match
    {
        case "filtmode" => chip.toggleExcludeMode()
        case "md" => chip.toggleMetaMode()
        case "nbt" => chip.toggleNBTMode()
        case "ore" => chip.toggleOreMode()
        case "grp" => chip.shiftDamageGroup()
        case "hide" => chip.shiftHiding()
    }
}

class GuiChipOrient(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipOrientation](cont, prev)
{
    val sideWidget = new JWidgetSideSelect(20 ,20, 50, 50, 40)
    {
        override def onSideChanged(s:Int)
        {
            val mask = sideMask
            for (i <- 0 until 6) if ((mask&1<<i) != 0)
            {
                chip.extractOrient = i
                return
            }
            chip.extractOrient = -1
        }
    }.setSideHighlighting(true).setTile(new TileEntityChest).setExclusive(true)

    if (chip.extractOrient >= 0) sideWidget.setSideMask(1<<chip.extractOrient)

    private val names = Seq("bottom", "top", "North", "South", "West", "East")

    override def drawBackExtra(mouse:Point, frame:Float)
    {
        val xOff = 90
        fontRenderer.drawString("Extraction is", xOff, 20, Colors_old.WHITE.rgb, true)
        if (chip.extractOrient == -1) fontRenderer.drawString("not simulated", xOff, 30, Colors_old.WHITE.rgb, true)
        else
        {
            fontRenderer.drawString("simulated from", xOff, 30, Colors_old.WHITE.rgb, true)
            fontRenderer.drawString("the " + names(chip.extractOrient), xOff, 40, Colors_old.WHITE.rgb, true)
        }
    }

    override def runInit_Impl()
    {
        add(sideWidget)
    }
}

class GuiChipPriority(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipPriority](cont, prev)
{
    override def drawBackExtra(mouse:Point, frame:Float)
    {
        FontUtils.drawCenteredString(chip.preference.toString, 88, 38, Colors_old.WHITE.rgb)
        if (chip.enablePriorityFlag) fontRenderer.drawStringWithShadow("Enabled", 98, 68, Colors_old.WHITE.rgb)
    }

    val check = new WidgetCheckBox(88, 72, chip.priorityFlag).setAction("p")
    override def runInit_Impl()
    {
        add(new WidgetButtonMC(82, 22, 12, 12).setText("+").setAction("u"))
        add(new WidgetButtonMC(82, 50, 12, 12).setText("-").setAction("d"))
        if (chip.enablePriorityFlag) add(new WidgetCheckBox(88, 72, chip.priorityFlag).setAction("p"))
    }

    override def receiveMessage_Impl(message:String) = message match
    {
        case "u" => chip.prefUp()
        case "d" => chip.prefDown()
        case "p" => chip.priorityFlag = check.state
    }
}

class GuiChipStock(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipStock](cont, prev)
{
    override def drawBackExtra(mouse:Point, frame:Float)
    {
        for ((x, y) <- GuiLib.createSlotGrid(20, 15, 3, 3, 0, 0))
            GuiLib.drawSlotBackground(x-1, y-1)
    }

    override def runInit_Impl()
    {
        add(new WidgetButtonIcon(150, 16, 14, 14)
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                drawTexturedModalRect(x, y, if (chip.requestWhenEmpty) 97 else 81, 102, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+="Fill mode"
                list+=(EnumChatFormatting.GRAY+"refill when items "+(if (chip.requestWhenEmpty) "empty" else "missing"))
            }
        }.setAction("fillmode"))
    }

    override def receiveMessage_Impl(message:String) = message match
    {
        case "fillmode" => chip.shiftRequestMode()
    }
}

class GuiChipCraftMatrix(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipCrafter](cont, prev)
{
    val tableResource = new ResourceLocation("textures/gui/container/crafting_table.png")

    override def drawBackExtra(mouse:Point, frame:Float)
    {
        CCRenderState.changeTexture(tableResource)
        drawTexturedModalRect(15, 10, 20, 12, 146, 62)
    }
}

class GuiChipCraftExt(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipCrafter](cont, prev)
{
    override def drawBackExtra(mouse:Point, frame:Float)
    {
        var index = 0
        for ((x, y) <- GuiLib.createGrid(xSize/2-40, 6, 3, 3, 28, 25))
        {
            if (chip.maxExtensions >= index)
            {
                val ext = chip.extIndex(index)
                if (ext >= 0)
                    Gui.drawRect(x+2, y, x+2+20, y+18, Colors_old.get(ext).argb)
                else drawCenteredString(fontRenderer, "off", x+12, y+8, Colors_old.WHITE.rgba)
            }
            else drawCenteredString(fontRenderer, "-", x+12, y+8, Colors_old.GREY.rgba)
            index += 1
        }
    }

    override def runInit_Impl()
    {
        var index = 0
        import scala.util.control.Breaks._
        for ((x, y) <- GuiLib.createGrid(xSize/2-40, 6, 3, 3, 24+4, 24+1)) breakable
        {
            if (chip.maxExtensions >= index)
            {
                add(new WidgetButtonMC(x, y, 24, 6).setAction(index+"u"))
                add(new WidgetButtonMC(x, y+18, 24, 6).setAction(index+"d"))
            }
            else break()
            index += 1
        }
    }

    override def receiveMessage_Impl(message:String)
    {
        val index = Integer.parseInt(message.substring(0, 1))

        if (message.substring(1) == "u") chip.extUp(index)
        else chip.extDown(index)
    }
}