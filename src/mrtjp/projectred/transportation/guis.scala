package mrtjp.projectred.transportation

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.FontUtils
import codechicken.lib.vec.BlockCoord
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors_old
import mrtjp.core.gui._
import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.core.resource.ResourceLib
import mrtjp.core.vec.Point
import mrtjp.projectred.core.libmc._
import net.minecraft.client.gui.Gui
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Container
import net.minecraft.util.EnumChatFormatting
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11

import scala.collection.mutable.ListBuffer

class GuiCraftingPipe(container:Container, pipe:RoutedCraftingPipePart) extends WidgetGui(container, 176, 220)
{
    override def receiveMessage_Impl(message:String)
    {
        val packet = new PacketCustom(TransportationCPH.channel, TransportationCPH.gui_CraftingPipe_action)
        packet.writeCoord(new BlockCoord(pipe.tile))
        packet.writeString(message)
        packet.sendToServer()
    }

    override def runInit_Impl()
    {
        add(new WidgetButtonMC(138, 12, 20, 14).setText("+").setAction("up"))
        add(new WidgetButtonMC(92, 12, 20, 14).setText("-").setAction("down"))
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiPipeCrafting.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
        FontUtils.drawCenteredString(""+pipe.priority, 126, 15, Colors_old.BLACK.rgb)
        GuiLib.drawPlayerInvBackground(8, 138)

        var color = 0
        ResourceLib.guiExtras.bind()

        for ((x, y) <- GuiLib.createSlotGrid(8, 108, 9, 1, 0, 0))
        {
            GL11.glColor4f(1, 1, 1, 1)
            drawTexturedModalRect(x, y, 1, 11, 16, 16)
            Gui.drawRect(x+4, y-2, x+4+8, y, Colors_old.get(color).argb)
            color += 1
        }
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiPipeCrafting.bind()
        val oldZ = zLevel
        zLevel = 300
        var i = 0
        for ((x, y) <- GuiLib.createSlotGrid(20, 12, 2, 4, 20, 0))
        {
            val u = 178
            val v = if (inventorySlots.getSlot(i).getStack == null) 107 else 85
            i += 1
            drawTexturedModalRect(x-5, y-2, u, v, 25, 20)
        }
        zLevel = oldZ
    }
}

object GuiCraftingPipe extends TGuiBuilder
{
    override def getID = TransportationProxy.guiIDCraftingPipe

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        PRLib.getMultiPart(player.worldObj, data.readCoord(), 6) match
        {
            case pipe:RoutedCraftingPipePart =>
                new GuiCraftingPipe(pipe.createContainer(player), pipe)
            case _ => null
        }
    }
}

class GuiExtensionPipe(container:Container, id:String) extends WidgetGui(container)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, zLevel)
        GuiLib.drawPlayerInvBackground(8, 84)

        fontRenderer.drawString("Extension ID:", 10, 10, 0xff000000)

        var i = 0
        for (s <- id.split("-"))
        {
            fontRenderer.drawString(s, 10, 25+10*i, 0xff000000)
            i+=1
        }

        GuiLib.drawSlotBackground(133, 19)
        GuiLib.drawSlotBackground(133, 49)
        ResourceLib.guiExtras.bind()
        drawTexturedModalRect(134, 20, 1, 11, 16, 16)
    }
}

object GuiExtensionPipe extends TGuiBuilder
{
    override def getID = TransportationProxy.guiIDExtensionPipe

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val coord = data.readCoord()
        val id = data.readString()
        PRLib.getMultiPart(player.worldObj, coord, 6) match
        {
            case pipe:RoutedExtensionPipePart =>
                new GuiExtensionPipe(pipe.createContainer(player), id)
            case _ => null
        }
    }
}

class GuiInterfacePipe(container:Container, pipe:RoutedInterfacePipePart) extends WidgetGui(container, 176, 200)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiPipeInterface.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
        GuiLib.drawPlayerInvBackground(8, 118)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiPipeInterface.bind()
        val oldZ = zLevel
        zLevel = 300

        for (i <- 0 until 4)
        {
            val x = 19
            val y = 10+i*26
            val u = 178
            val v = if (inventorySlots.getSlot(i).getStack == null) 107 else 85
            drawTexturedModalRect(x, y, u, v, 25, 20)
        }
        zLevel = oldZ
    }
}

object GuiInterfacePipe extends TGuiBuilder
{
    override def getID = TransportationProxy.guiIDInterfacePipe

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val coord = data.readCoord()
        PRLib.getMultiPart(player.worldObj, coord, 6) match
        {
            case pipe:RoutedInterfacePipePart =>
                new GuiInterfacePipe(pipe.createContainer(player), pipe)
            case _ => null
        }
    }
}

class GuiRequester(pipe:IWorldRequester) extends WidgetGui(280, 230)
{
    var itemList = new WidgetItemList(xSize/2-220/2, 10, 220, 140)

    var textFilter = new WidgetTextBox(xSize/2-150/2, 185, 150, 16)
    {
        override def onTextChanged(oldText:String) {itemList.setNewFilter(text)}
    }.setMaxCharCount(24)

    var itemCount = new WidgetTextBox(xSize/2-50/2, 205, 50, 16, "1")
    {
        override def mouseScrolled_Impl(p:Point, dir:Int, consumed:Boolean) =
        {
            if (!consumed && bounds.intersects(p))
            {
                if (dir > 0) countUp()
                else if (dir < 0) countDown()
                true
            }
            else false
        }

        override def onFocusChanged()
        {
            if (text == null || text.isEmpty) setText("1")
        }
    }.setAllowedChars("0123456789").setMaxCharCount(7)

    var pull = new WidgetCheckBox(230, 170, true).setAction("refrsh")
    var craft = new WidgetCheckBox(230, 190, true).setAction("refrsh")
    var partials = new WidgetCheckBox(230, 210, false)

    override def forwardClosing = !textFilter.isFocused

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, zLevel)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        fontRenderer.drawStringWithShadow("Pull", 240, 166, Colors_old.WHITE.rgb)
        fontRenderer.drawStringWithShadow("Craft", 240, 186, Colors_old.WHITE.rgb)
        fontRenderer.drawStringWithShadow("Parials", 240, 206, Colors_old.WHITE.rgb)
    }

    override def runInit_Impl()
    {
        add(itemList)
        add(textFilter)
        add(itemCount)
        add(pull)
        add(craft)
        add(partials)
        add(new WidgetButtonMC(10, 185, 50, 16).setAction("refrsh").setText("Re-poll"))
        add(new WidgetButtonMC(10, 205, 50, 16).setAction("req").setText("Submit"))
        add(new WidgetButtonMC(95, 205, 16, 16).setAction("-").setText("-"))
        add(new WidgetButtonMC(170, 205, 16, 16).setAction("+").setText("+"))
        add(new WidgetButtonMC(85, 152, 16, 16).setAction("p-").setText("-"))
        add(new WidgetButtonMC(180, 152, 16, 16).setAction("p+").setText("+"))
        add(new WidgetButtonMC(190, 205, 24, 16).setAction("all").setText("All"))
        askForListRefresh()
    }

    private def sendItemRequest()
    {
        val count = itemCount.text
        if (count == null || count.isEmpty) return

        val amount = Integer.parseInt(count)
        if (amount <= 0) return

        val request = itemList.getSelected
        if (request != null)
        {
            val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_Request_submit)
            packet.writeCoord(new BlockCoord(pipe.getContainer.tile))
            packet.writeBoolean(pull.state)
            packet.writeBoolean(craft.state)
            packet.writeBoolean(partials.state)
            packet.writeItemStack(request.key.makeStack(amount), true)
            packet.sendToServer()
        }
    }

    private def askForListRefresh()
    {
        val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_Request_listRefresh)
        packet.writeCoord(new BlockCoord(pipe.getContainer.tile))
        packet.writeBoolean(pull.state)
        packet.writeBoolean(craft.state)
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
        val s = itemCount.text
        if (s != null && !s.isEmpty) current = Integer.parseInt(s)

        var newCount = 0
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) newCount = current + 10
        else newCount = current + 1

        if (String.valueOf(newCount).length <= itemCount.maxStringLength)
            itemCount.setText(String.valueOf(newCount))
    }

    private def countDown()
    {
        var current = 0
        val s = itemCount.text
        if (s != null && !s.isEmpty) current = Integer.parseInt(s)

        var newCount = 0
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) newCount = current - 10
        else newCount = current - 1
        newCount = Math.max(1, newCount)

        if (String.valueOf(newCount).length <= itemCount.maxStringLength) itemCount.setText(String.valueOf(newCount))
    }

    override def receiveMessage_Impl(message:String) = message match
    {
        case "req" => sendItemRequest()
        case "refrsh" =>
            itemList.resetDownloadStats()
            askForListRefresh()
        case "+" => countUp()
        case "-" => countDown()
        case "p+" => itemList.pageUp()
        case "p-" => itemList.pageDown()
        case "all" => if (itemList.getSelected != null) itemCount.setText(String.valueOf(Math.max(1, itemList.getSelected.stackSize)))
        case _ => sendAction(message)
    }

    def receiveContentList(content:Map[ItemKey, Int])
    {
        itemList.setDisplayList(content.map(p => ItemKeyStack.get(p._1, p._2)).toVector.sorted)
    }
}

class GuiFirewallPipe(slots:Container, pipe:RoutedFirewallPipe) extends WidgetGui(slots, 276, 200)
{
    private[this] class SelectButton(x:Int, y:Int, f: => Boolean, desc:String) extends WidgetButtonIcon(x, y, 14, 14)
    {
        override def drawButton(mouseover:Boolean)
        {
            ResourceLib.guiExtras.bind()
            drawTexturedModalRect(x, y, if (f) 33 else 49, 134, 14, 14)
        }

        override def buildTooltip(list:ListBuffer[String])
        {
            list += (EnumChatFormatting.GRAY+desc)
        }
    }

    override def runInit_Impl()
    {
        add(new WidgetButtonIcon(134, 8, 14, 14)
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                drawTexturedModalRect(x, y, if (pipe.filtExclude) 1 else 17, 102, 14, 14)
            }

            override def buildTooltip(list:ListBuffer[String])
            {
                list+=(EnumChatFormatting.GRAY+"Items are "+(if (pipe.filtExclude) "blacklisted" else "whitelisted"))
            }
        }.setAction("excl"))

        add(new SelectButton(183-7, 130-7, pipe.allowRoute, "Push routing").setAction("route"))
        add(new SelectButton(208-7, 130-7, pipe.allowBroadcast, "Pulling").setAction("broad"))
        add(new SelectButton(233-7, 130-7, pipe.allowCrafting, "Crafting").setAction("craft"))
        add(new SelectButton(258-7, 130-7, pipe.allowController, "Controller access").setAction("cont"))
    }

    override def receiveMessage_Impl(message:String)
    {
        new PacketCustom(TransportationCPH.channel, TransportationCPH.gui_FirewallPipe_action)
            .writeCoord(new BlockCoord(pipe.tile)).writeString(message).sendToServer()
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, zLevel)
        GuiLib.drawPlayerInvBackground(8, 120)
        for ((x, y) <- GuiLib.createSlotGrid(8, 8, 7, 5, 0, 0))
            GuiLib.drawSlotBackground(x-1, y-1)

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
            GuiLib.drawLine(inX, inY, dx, dy)

            val dx2 = dx
            val dy2 = dy-flowY*0.25D
            GuiLib.drawLine(dx, dy, dx2, dy2)

            val dx3 = dx2
            val dy3 = dy-flowY
            if (flags(i)) GuiLib.drawLine(dx2, dy2, dx3, dy3)
            else GuiLib.drawLine(dx2, dy2, dx3, dy3, Colors_old.GREY.rgb)

            if (flags(i)) GuiLib.drawLine(dx3, dy3, outX, outY)
            else GuiLib.drawLine(dx3, dy3, outX, outY, Colors_old.GREY.rgb)
        }

        GuiLib.drawLine(inX, inY, inX, inY+inOut)
        GuiLib.drawLine(outX, outY, outX, outY-inOut)
        GuiLib.drawLine(141, 15, outX, 15)
    }
}

object GuiFirewallPipe extends TGuiBuilder
{
    override def getID = TransportationProxy.guiIDFirewallPipe

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val coord = data.readCoord()
        val filtExclude = data.readBoolean()
        val allowRoute = data.readBoolean()
        val allowBroadcast = data.readBoolean()
        val allowCrafting = data.readBoolean()
        val allowController = data.readBoolean()
        PRLib.getMultiPart(player.worldObj, coord, 6) match
        {
            case pipe:RoutedFirewallPipe =>
                pipe.filtExclude = filtExclude
                pipe.allowRoute = allowRoute
                pipe.allowBroadcast = allowBroadcast
                pipe.allowCrafting = allowCrafting
                pipe.allowController = allowController
                new GuiFirewallPipe(pipe.createContainer(player), pipe)
            case _ => null
        }
    }
}