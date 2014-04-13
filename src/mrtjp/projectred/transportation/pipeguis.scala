package mrtjp.projectred.transportation

import net.minecraft.inventory.Container
import mrtjp.projectred.core.inventory._
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.BlockCoord
import codechicken.lib.render.{FontUtils, CCRenderState}
import mrtjp.projectred.core.{BasicGuiUtils, PRColors}
import org.lwjgl.opengl.GL11
import net.minecraft.client.gui.{GuiButton, Gui}
import net.minecraft.util.{EnumChatFormatting, MathHelper, ResourceLocation}
import org.lwjgl.input.Keyboard
import java.util
import mrtjp.projectred.core.utils.{ItemKeyStack, ItemKey}
import java.util.Collections
import mrtjp.projectred.core.inventory.SpecialContainer.SlotExtended
import cpw.mods.fml.client.FMLClientHandler
import net.minecraft.client.renderer.Tessellator
import scala.util.Random
import scala.collection.mutable.ListBuffer

class GuiCraftingPipe(container:Container, pipe:RoutedCraftingPipePart) extends SpecialGuiContainer(container, null, 176, 220)
{
    override def actionPerformed(ident:String)
    {
        val packet = new PacketCustom(TransportationCPH.channel, TransportationCPH.gui_CraftingPipe_action)
        packet.writeCoord(new BlockCoord(pipe.tile))
        packet.writeString(ident)
        packet.sendToServer()
    }

    override def addWidgets()
    {
        add(new JWidgetButton(138, 12, 20, 14).setText("+").setActionCommand("up"))
        add(new JWidgetButton(92, 12, 20, 14).setText("-").setActionCommand("down"))
    }

    override def drawBackground()
    {
        CCRenderState.changeTexture(GuiCraftingPipe.resource)
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
        FontUtils.drawCenteredString("" + pipe.priority, 126, 15, PRColors.BLACK.rgb)
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 138)

        var color = 0
        CCRenderState.changeTexture(SpecialGuiContainer.guiExtras)

        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createSlotArray(8, 108, 9, 1, 0, 0))
        {
            GL11.glColor4f(1, 1, 1, 1)
            drawTexturedModalRect(p.get1, p.get2, 1, 11, 16, 16)
            val x = p.get1 + 4
            val y = p.get2 - 2
            Gui.drawRect(x, y, x + 8, y + 2, PRColors.get(color).argb)
            color += 1
        }
    }

    override def drawForeground()
    {
        CCRenderState.changeTexture(GuiCraftingPipe.resource)
        val oldZ = zLevel
        zLevel = 300
        var i = 0
        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createSlotArray(20, 12, 2, 4, 20, 0))
        {
            val x = p.get1 - 5
            val y = p.get2 - 2
            val u = 178
            val v = if (inventorySlots.getSlot(i).getStack == null) 107 else 85
            i += 1
            drawTexturedModalRect(x, y, u, v, 25, 20)
        }
        zLevel = oldZ
    }
}

object GuiCraftingPipe
{
    val resource = new ResourceLocation("projectred:textures/gui/guicraftingpipe.png")
}

class GuiExtensionPipe(container:Container, id:String) extends SpecialGuiContainer(container, null)
{
    override def drawBackground()
    {
        BasicGuiUtils.drawGuiBox(0, 0, xSize, ySize, zLevel)
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 84)

        fontRenderer.drawString("Extension ID:", 10, 10, 0xff000000)

        var i = 0
        for (s <- id.split("-"))
        {
            fontRenderer.drawString(s, 10, 25+10*i, 0xff000000)
            i+=1
        }

        BasicGuiUtils.drawSlotBackground(mc, 133, 19)
        BasicGuiUtils.drawSlotBackground(mc, 133, 49)
        CCRenderState.changeTexture(SpecialGuiContainer.guiExtras)
        drawTexturedModalRect(134, 20, 1, 11, 16, 16)
    }
}

class GuiInterfacePipe(slots:Container, pipe:RoutedInterfacePipePart) extends SpecialGuiContainer(slots, null, 176, 200)
{
    override def drawBackground()
    {
        CCRenderState.changeTexture(GuiInterfacePipe.resource)
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 118)
    }

    override def drawForeground()
    {
        CCRenderState.changeTexture(GuiInterfacePipe.resource)
        val oldZ:Float = zLevel
        zLevel = 300

        for (i <- 0 until 4)
        {
            val x:Int = 19
            val y:Int = 10 + i * 26
            val u:Int = 178
            val v:Int = if (inventorySlots.getSlot(i).getStack == null) 107 else 85
            drawTexturedModalRect(x, y, u, v, 25, 20)
        }
        zLevel = oldZ
    }
}

object GuiInterfacePipe
{
    val resource = new ResourceLocation("projectred:textures/gui/guiinterfacepipe.png")
}

class GuiRequester(pipe:IWorldRequester) extends SpecialGuiContainer(280, 230)
{
    var itemList = new WidgetItemSelection(xSize/2-220/2, 10, 220, 140)

    var textFilter = new WidgetTextBox(xSize/2-150/2, 185, 150, 16, "")
    {
        override def onTextChanged(oldText:String)
        {
            itemList.setNewFilter(getText)
        }
    }.setMaxStringLength(24)

    var itemCount = new WidgetTextBox(xSize/2-50/2, 205, 50, 16, "1")
    {
        override def mouseScrolled(x:Int, y:Int, scroll:Int)
        {
            if (pointInside(x, y))
            {
                if (scroll > 0) countUp()
                else if (scroll < 0) countDown()
            }
        }

        override def onFocusChanged()
        {
            if (getText == null || getText.isEmpty)
            {
                setText("1")
            }
        }
    }.setAllowedCharacters("0123456789").setMaxStringLength(7)

    var pull = new WidgetCheckBox(230, 170, true)
    {
        override def onStateChanged(oldState:Boolean)
        {
            itemList.resetDownloadStats()
            askForListRefresh()
        }
    }

    var craft = new WidgetCheckBox(230, 190, true)
    {
        override def onStateChanged(oldState:Boolean)
        {
            itemList.resetDownloadStats()
            askForListRefresh()
        }
    }

    var partials = new WidgetCheckBox(230, 210, false)


    override def forwardClosingKey = !textFilter.isFocused

    override def drawBackground()
    {
        BasicGuiUtils.drawGuiBox(0, 0, xSize, ySize, zLevel)
    }

    override def drawForeground()
    {
        fontRenderer.drawStringWithShadow("Pull", 240, 166, PRColors.WHITE.rgb)
        fontRenderer.drawStringWithShadow("Craft", 240, 186, PRColors.WHITE.rgb)
        fontRenderer.drawStringWithShadow("Parials", 240, 206, PRColors.WHITE.rgb)
    }

    override def addWidgets()
    {
        add(itemList)
        add(textFilter)
        add(itemCount)
        add(pull)
        add(craft)
        add(partials)
        add(new JWidgetButton(10, 185, 50, 16).setActionCommand("refrsh").setText("Re-poll"))
        add(new JWidgetButton(10, 205, 50, 16).setActionCommand("req").setText("Submit"))
        add(new JWidgetButton(95, 205, 16, 16).setActionCommand("-").setText("-"))
        add(new JWidgetButton(170, 205, 16, 16).setActionCommand("+").setText("+"))
        add(new JWidgetButton(85, 152, 16, 16).setActionCommand("p-").setText("-"))
        add(new JWidgetButton(180, 152, 16, 16).setActionCommand("p+").setText("+"))
        add(new JWidgetButton(190, 205, 24, 16).setActionCommand("all").setText("All"))
        askForListRefresh()
    }

    private def sendItemRequest()
    {
        val count = itemCount.getText
        if (count == null || count.isEmpty) return

        val amount = Integer.parseInt(count)
        if (amount <= 0) return

        val request = itemList.getSelection
        if (request != null)
        {
            val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_Request_submit)
            packet.writeCoord(new BlockCoord(pipe.getContainer.tile))
            packet.writeBoolean(pull.getChecked)
            packet.writeBoolean(craft.getChecked)
            packet.writeBoolean(partials.getChecked)
            packet.writeItemStack(request.key.makeStack(amount), true)
            packet.sendToServer()
        }
    }

    private def askForListRefresh()
    {
        val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_Request_listRefresh)
        packet.writeCoord(new BlockCoord(pipe.getContainer.tile))
        packet.writeBoolean(pull.getChecked)
        packet.writeBoolean(craft.getChecked)
        packet.sendToServer()
    }

    private def sendAction(ident:String)
    {
        val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_Request_action)
        packet.writeCoord(new BlockCoord(pipe.getContainer.tile))
        packet.writeString(ident)
        packet.sendToServer()
    }

    private def countUp()
    {
        var current = 0
        val s = itemCount.getText
        if (s != null && !s.isEmpty) current = Integer.parseInt(s)

        var newCount = 0
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) newCount = current + 10
        else newCount = current + 1

        if (String.valueOf(newCount).length <= itemCount.maxStringLength) itemCount.setText(String.valueOf(newCount))
    }

    private def countDown()
    {
        var current = 0
        val s = itemCount.getText
        if (s != null && !s.isEmpty) current = Integer.parseInt(s)

        var newCount = 0
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) newCount = current - 10
        else newCount = current - 1
        newCount = Math.max(1, newCount)

        if (String.valueOf(newCount).length <= itemCount.maxStringLength) itemCount.setText(String.valueOf(newCount))
    }

    override def actionPerformed(ident:String) = ident match
    {
        case "req" => sendItemRequest()
        case "refrsh" =>
            itemList.resetDownloadStats()
            askForListRefresh()
        case "+" => countUp()
        case "-" => countDown()
        case "p+" => itemList.pageUp()
        case "p-" => itemList.pageDown()
        case "all" => if (itemList.getSelection != null) itemCount.setText(String.valueOf(Math.max(1, itemList.getSelection.stackSize)))
        case _ => sendAction(ident)
    }

    def receiveContentList(content:util.Map[ItemKey, Integer])
    {
        val list = new util.ArrayList[ItemKeyStack](content.size)
        import scala.collection.JavaConversions._
        for (entry <- content.entrySet) list.add(ItemKeyStack.get(entry.getKey, entry.getValue))
        Collections.sort(list)
        itemList.setDisplayList(list)
    }
}

class GuiFirewallPipe(slots:Container, pipe:RoutedFirewallPipe) extends SpecialGuiContainer(slots, null, 276, 200)
{
    private[this] class SelectButton(x:Int, y:Int, f: => Boolean, desc:String) extends WidgetButton(x, y, 14, 14) with TButtonMCStyle
    {
        override def drawButton(mouseover:Boolean)
        {
            CCRenderState.changeTexture(GhostWidget.guiExtras)
            drawTexturedModalRect(x, y, if (f) 33 else 49, 134, 14, 14)
        }

        override def buildTooltip(list:ListBuffer[String])
        {
            list += (EnumChatFormatting.GRAY+desc)
        }
    }

    override def addWidgets()
    {
        add(new WidgetButton(134, 8, 14, 14) with TButtonMCStyle
        {
            override def drawButton(mouseover:Boolean)
            {
                CCRenderState.changeTexture(GhostWidget.guiExtras)
                drawTexturedModalRect(x, y, if (pipe.filtExclude) 1 else 17, 102, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+=(EnumChatFormatting.GRAY+"Items are "+(if (pipe.filtExclude) "blacklisted" else "whitelisted"))
            }
        }.setActionCommand("excl"))

        add(new SelectButton(183-7, 130-7, pipe.allowRoute, "Push routing").setActionCommand("route"))
        add(new SelectButton(208-7, 130-7, pipe.allowBroadcast, "Pulling").setActionCommand("broad"))
        add(new SelectButton(233-7, 130-7, pipe.allowCrafting, "Crafting").setActionCommand("craft"))
        add(new SelectButton(258-7, 130-7, pipe.allowController, "Controller access").setActionCommand("cont"))
    }

    override def actionPerformed(ident:String)
    {
        new PacketCustom(TransportationCPH.channel, TransportationCPH.gui_FirewallPipe_action)
            .writeCoord(new BlockCoord(pipe.tile)).writeString(ident).sendToServer()
    }

    override def drawBackground()
    {
        BasicGuiUtils.drawGuiBox(0, 0, xSize, ySize, zLevel)
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 120)

        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createSlotArray(8, 8, 7, 5, 0, 0))
            BasicGuiUtils.drawSlotBackground(mc, p.get1-1, p.get2-1)

        val inX = 221
        val inY = 180
        val spread = 25
        val rootY = 20
        val flowY = 120

        val outY = inY-2*rootY-flowY
        val outX = inX
        val inOut = 15

        val flags = Seq(pipe.allowRoute, pipe.allowBroadcast, pipe.allowCrafting, pipe.allowController)
        for (i <- 0 until 4)
        {
            val dx = (inX-(1.5D*spread))+spread*i
            val dy = inY-rootY
            BasicGuiUtils.drawLine(inX, inY, dx, dy)

            val dx2 = dx
            val dy2 = dy-flowY*0.25D
            BasicGuiUtils.drawLine(dx, dy, dx2, dy2)

            val dx3 = dx2
            val dy3 = dy-flowY
            if (flags(i)) BasicGuiUtils.drawLine(dx2, dy2, dx3, dy3)
            else BasicGuiUtils.drawLine(dx2, dy2, dx3, dy3, PRColors.GREY.rgb)

            if (flags(i)) BasicGuiUtils.drawLine(dx3, dy3, outX, outY)
            else BasicGuiUtils.drawLine(dx3, dy3, outX, outY, PRColors.GREY.rgb)
        }

        BasicGuiUtils.drawLine(inX, inY, inX, inY+inOut)
        BasicGuiUtils.drawLine(outX, outY, outX, outY-inOut)
        BasicGuiUtils.drawLine(141, 15, outX, 15)
    }
}