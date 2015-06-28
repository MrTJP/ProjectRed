package mrtjp.projectred.transportation

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.{CCRenderState, FontUtils, TextureUtils}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.resource.ResourceLib
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.core.libmc.PRResources
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal
import net.minecraft.client.gui.{Gui, GuiScreen}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Slot
import net.minecraft.util.{EnumChatFormatting, ResourceLocation}
import org.lwjgl.opengl.GL11

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

class ChipContainer(player:EntityPlayer, var chip:RoutingChip) extends NodeContainer
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
        super.addSlotToContainer(slot)
        if (slot.getSlotIndex == this.slot && slot.inventory == player.inventory)
            slot.asInstanceOf[Slot3].canRemoveDelegate = {() => false}
        slot
    }
}

abstract class GuiChipContainer[T <: RoutingChip](cont:ChipContainer, prev:GuiScreen) extends NodeGui(cont)
{
    setJumpBack(prev)

    def chip:T = cont.chip.asInstanceOf[T]
    def cleanContainer = cont.getNewInstance

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
    {
        keycode == mcInst.thePlayer.inventory.currentItem+2
    }

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
    override def onAddedToParent_Impl()
    {
        if (chip.isInstanceOf[TChipFilter] && chip.asInstanceOf[TChipFilter].enableFilter)
        {
            val dot = DotSelectNode.centered(85, 34)
            dot.tooltipBuilder = { list =>
                list += "Filter"; chip.asInstanceOf[TChipFilter].addFilterInfo(list)
            }
            dot.clickDelegate = {() =>
                val c = cleanContainer
                c.addPlayerInv(8, 86)
                val c2 = chip.asInstanceOf[TChipFilter]
                var s = 0
                for ((x, y) <- GuiLib.createSlotGrid(20, 15, 3, 3, 0, 0))
                {
                    val slot = new Slot3(c2.filter, s, x, y)
                    slot.phantomSlot = true
                    c.addSlotToContainer(slot)
                    s += 1
                }
                jumpTo(new GuiChipFilter(c, this), true)
            }
            addChild(dot)
        }

        if (chip.isInstanceOf[TChipOrientation])
        {
            val dot = DotSelectNode.centered(100, 50)
            dot.tooltipBuilder = { list =>
                list += "Orientation"
                chip.asInstanceOf[TChipOrientation].addOrientInfo(list)
            }
            dot.clickDelegate = { () =>
                val c = cleanContainer
                c.addPlayerInv(8, 86)
                jumpTo(new GuiChipOrient(c, this), true)
            }
            addChild(dot)
        }

        if (chip.isInstanceOf[TChipPriority] && chip.asInstanceOf[TChipPriority].prefScale > 0)
        {
            val dot = DotSelectNode.centered(76, 51)
            dot.tooltipBuilder = { list =>
                list += "Priority"
                chip.asInstanceOf[TChipPriority].addPriorityInfo(list)
            }
            dot.clickDelegate = { () =>
                val c = cleanContainer
                c.addPlayerInv(8, 86)
                jumpTo(new GuiChipPriority(c, this), true)
            }
            addChild(dot)
        }

        if (chip.isInstanceOf[TChipStock])
        {
            val dot = DotSelectNode.centered(90, 50)
            dot.tooltipBuilder = { list =>
                list += "Stock"
                chip.asInstanceOf[TChipStock].addStockInfo(list)
            }
            dot.clickDelegate = {() =>
                val c = cleanContainer
                c.addPlayerInv(8, 86)
                val c2 = chip.asInstanceOf[TChipStock]
                var s = 0
                for ((x, y) <- GuiLib.createSlotGrid(20, 15, 3, 3, 0, 0))
                {
                    val slot = new Slot3(c2.stock, s, x, y)
                    slot.phantomSlot = true
                    c.addSlotToContainer(slot)
                    s += 1
                }
                jumpTo(new GuiChipStock(c, this), true)
            }
            addChild(dot)
        }

        if (chip.isInstanceOf[TChipCrafter])
        {
            val chip2 = chip.asInstanceOf[TChipCrafter]
            def dot1 = DotSelectNode.centered(105, 42)
            dot1.tooltipBuilder = { list =>
                list += "Matrix"
                chip2.addMatrixInfo(list)
            }
            dot1.clickDelegate = {() =>
                val c = cleanContainer
                c.addPlayerInv(8, 86)
                var s = 0
                for ((x, y) <- GuiLib.createSlotGrid(25, 15, 3, 3, 0, 0))
                {
                    val slot = new Slot3(chip2.matrix, s, x, y)
                    slot.phantomSlot = true
                    c.addSlotToContainer(slot)
                    s += 1
                }
                val slot = new Slot3(chip2.matrix, s, 119, 33)
                slot.phantomSlot = true
                c.addSlotToContainer(slot)
                jumpTo(new GuiChipCraftMatrix(c, this), true)
            }
            addChild(dot1)

            if (chip2.maxExtensions > 0)
            {
                val dot2 = DotSelectNode.centered(90, 32)
                dot2.tooltipBuilder = { list =>
                    list += "Extensions"
                    chip2.addExtInfo(list)
                }
                dot2.clickDelegate = {() =>
                    val c = cleanContainer
                    c.addPlayerInv(8, 86)
                    jumpTo(new GuiChipCraftExt(c, this), true)
                }
                addChild(dot2)
            }
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

    override def onAddedToParent_Impl()
    {
        if (chip.enableFilter)
        {
            val b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(position.x, position.y, if(chip.filterExclude) 1 else 17, 102, 14, 14)
                }
            }
            b.position = Point(130, 16)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Filter mode"
                list += (EnumChatFormatting.GRAY + "Items are " + (if(chip.filterExclude) "blacklisted" else "whitelisted"))
            }
            b.clickDelegate = { () => chip.toggleExcludeMode() }
            addChild(b)
        }

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
            b.position = Point(150, 16)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Metadata matching"
                list += (EnumChatFormatting.GRAY+"Meta is "+(if (chip.metaMatch) "checked" else "ignored"))
            }
            b.clickDelegate = {() => chip.toggleMetaMode()}
            addChild(b)

            b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(position.x, position.y, if (chip.nbtMatch) 33 else 49, 102, 14, 14)
                }
            }
            b.position = Point(150, 32)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "NBT matching"
                list += (EnumChatFormatting.GRAY+"NBT is "+(if (chip.nbtMatch) "checked" else "ignored"))
            }
            b.clickDelegate = {() => chip.toggleNBTMode()}
            addChild(b)

            b = new IconButtonNode
            {
                override def drawButton(mouseover:Boolean)
                {
                    ResourceLib.guiExtras.bind()
                    drawTexturedModalRect(position.x, position.y, if (chip.oreMatch) 81 else 97, 118, 14, 14)
                }
            }
            b.position = Point(150, 48)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Ore Dictionary matching"
                list += (EnumChatFormatting.GRAY+"Ore Dictionary is "+(if (chip.oreMatch) "checked" else "ignored"))
            }
            b.clickDelegate = {() => chip.toggleOreMode()}
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
            b.position = Point(125, 35)
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
            b.clickDelegate = {() => chip.shiftDamageGroup()}
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
            b.position = Point(114, 16)
            b.size = Size(14, 14)
            b.tooltipBuilder = { list =>
                list += "Item hiding"
                list += (EnumChatFormatting.GRAY+"Hide "+(if (chip.hideMode == 0) "nothing" else chip.hide(chip.hideMode)))
            }
            b.clickDelegate = {() => chip.shiftHiding()}
            addChild(b)
        }
    }
}

class GuiChipOrient(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipOrientation](cont, prev)
{
    val sideWidget = new SideSelectNode(20, 15, 50, 50)
    {
        override def onSideChanged(oldside:Int)
        {
            chip.extractOrient = if (sides != 0) Integer.numberOfTrailingZeros(sides) else -1
        }
    }

    sideWidget.exclusiveSides = true
    if (chip.extractOrient >= 0) sideWidget.sides = 1<<chip.extractOrient

    private val names = Seq("bottom", "top", "North", "South", "West", "East")

    override def drawBackExtra(mouse:Point, frame:Float)
    {
        val xOff = 90
        fontRenderer.drawString("Extraction is", xOff, 20, Colors.WHITE.rgb, true)
        if (chip.extractOrient == -1) fontRenderer.drawString("not simulated", xOff, 30, Colors.WHITE.rgb, true)
        else
        {
            fontRenderer.drawString("simulated from", xOff, 30, Colors.WHITE.rgb, true)
            fontRenderer.drawString("the " + names(chip.extractOrient), xOff, 40, Colors.WHITE.rgb, true)
        }
    }

    override def onAddedToParent_Impl()
    {
        addChild(sideWidget)
    }
}

class GuiChipPriority(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipPriority](cont, prev)
{
    override def drawBackExtra(mouse:Point, frame:Float)
    {
        FontUtils.drawCenteredString(chip.preference.toString, 88, 38, Colors.WHITE.rgb)
        if (chip.enablePriorityFlag) fontRenderer.drawStringWithShadow("Enabled", 98, 68, Colors.WHITE.rgb)
    }

    override def onAddedToParent_Impl()
    {
        val plus = new MCButtonNode
        plus.position = Point(82, 22)
        plus.size = Size(12, 12)
        plus.text = "+"
        plus.clickDelegate = {() => chip.prefUp()}
        addChild(plus)

        val minus = new MCButtonNode
        minus.position = Point(82, 50)
        minus.size = Size(12, 12)
        minus.text = "-"
        minus.clickDelegate = {() => chip.prefDown()}
        addChild(minus)

        if (chip.enablePriorityFlag)
        {
            val check = CheckBoxNode.centered(88, 72)
            check.state = chip.priorityFlag
            check.clickDelegate = {() => chip.priorityFlag = check.state}
            addChild(check)
        }
    }
}

class GuiChipStock(cont:ChipContainer, prev:GuiScreen) extends GuiChipContainer[TChipStock](cont, prev)
{
    override def drawBackExtra(mouse:Point, frame:Float)
    {
        for ((x, y) <- GuiLib.createSlotGrid(20, 15, 3, 3, 0, 0))
            GuiLib.drawSlotBackground(x-1, y-1)
    }

    override def onAddedToParent_Impl()
    {
        val b = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                drawTexturedModalRect(position.x, position.y, if (chip.requestWhenEmpty) 97 else 81, 102, 14, 14)
            }
        }
        b.position = Point(150, 16)
        b.size = Size(14, 14)
        b.tooltipBuilder = {list =>
            list += "Fill mode"
            list += (EnumChatFormatting.GRAY+"refill when items "+(if (chip.requestWhenEmpty) "empty" else "missing"))
        }
        b.clickDelegate = {() => chip.shiftRequestMode()}
        addChild(b)
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
                    Gui.drawRect(x+2, y, x+2+20, y+18, Colors(ext).argb)
                else drawCenteredString(fontRenderer, "off", x+12, y+8, Colors.WHITE.rgba)
            }
            else drawCenteredString(fontRenderer, "-", x+12, y+8, Colors.GREY.rgba)
            index += 1
        }
    }

    override def onAddedToParent_Impl()
    {
        var index = 0
        import scala.util.control.Breaks._
        for ((x, y) <- GuiLib.createGrid(xSize/2-40, 6, 3, 3, 24+4, 24+1)) breakable
        {
            if (chip.maxExtensions >= index)
            {
                val up = new MCButtonNode
                up.position = Point(x, y)
                up.size = Size(24, 6)
                up.clickDelegate = {() => chip.extUp(index)}
                addChild(up)

                val down = new MCButtonNode
                down.position = Point(x, y+18)
                down.size = Size(24, 6)
                down.clickDelegate = {() => chip.extDown(index)}
                addChild(down)
            }
            else break()
            index += 1
        }
    }
}