package mrtjp.projectred.transportation

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.FontUtils
import codechicken.lib.vec.BlockCoord
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.{Colors, Colors_old}
import mrtjp.core.gui._
import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.core.resource.ResourceLib
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.core.libmc._
import net.minecraft.client.gui.Gui
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Container
import net.minecraft.util.EnumChatFormatting
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11

class GuiCraftingPipe(container:Container, pipe:RoutedCraftingPipePart) extends NodeGui(container, 176, 220)
{
    override def onAddedToParent_Impl()
    {
        def sendClick(message:String)
        {
            val packet = new PacketCustom(TransportationCPH.channel, TransportationCPH.gui_CraftingPipe_action)
            packet.writeCoord(new BlockCoord(pipe.tile))
            packet.writeString(message)
            packet.sendToServer()
        }

        val up = new MCButtonNode
        up.position = Point(138, 12)
        up.size = Size(20, 14)
        up.text = "up"
        up.clickDelegate = {() => sendClick("up")}
        addChild(up)

        val down = new MCButtonNode
        down.position = Point(92, 12)
        down.size = Size(20, 14)
        down.text = "down"
        down.clickDelegate = {() => sendClick("down")}
        addChild(down)
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

class GuiExtensionPipe(container:Container, id:String) extends NodeGui(container)
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

class GuiInterfacePipe(container:Container, pipe:RoutedInterfacePipePart) extends NodeGui(container, 176, 200)
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

class GuiRequester(pipe:IWorldRequester) extends NodeGui(280, 230)
{
    var itemList = new NodeItemList(xSize/2-220/2, 10, 220, 140)

    var textFilter:SimpleTextboxNode = null

    var itemCount:SimpleTextboxNode = null

    var pull:CheckBoxNode = null//new NodeCheckBox(230, 170, true).setAction("refrsh")
    var craft:CheckBoxNode = null// = new NodeCheckBox(230, 190, true).setAction("refrsh")
    var partials:CheckBoxNode = null// = new NodeCheckBox(230, 210, false)

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, zLevel)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        fontRenderer.drawStringWithShadow("Pull", 240, 166, Colors.WHITE.rgb)
        fontRenderer.drawStringWithShadow("Craft", 240, 186, Colors.WHITE.rgb)
        fontRenderer.drawStringWithShadow("Parials", 240, 206, Colors.WHITE.rgb)
    }

    override def onAddedToParent_Impl()
    {
        addChild(itemList)

        textFilter = new SimpleTextboxNode
        textFilter.position = Point(size.width/2-150/2, 185)
        textFilter.size = Size(150, 16)
        textFilter.phantom = "filter results"
        textFilter.textChangedDelegate = {() => itemList.setNewFilter(textFilter.text)}
        addChild(textFilter)

        itemCount = new SimpleTextboxNode
        {
            override def mouseScrolled_Impl(p:Point, dir:Int, consumed:Boolean) =
            {
                if (!consumed && rayTest(p))
                {
                    if (dir > 0) countUp()
                    else if (dir < 0) countDown()
                    true
                }
                else false
            }
        }
        itemCount.position = Point(size.width/2-50/2, 205)
        itemCount.size = Size(50, 16)
        itemCount.text = "1"
        itemCount.phantom = "1"
        itemCount.allowedcharacters = "0123456789"
        itemCount.focusChangeDelegate = {() =>
            if (!itemCount.focused)
                if (itemCount.text.isEmpty || Integer.parseInt(itemCount.text) < 1)
                    itemCount.text = "1"
        }
        addChild(itemCount)

        pull = CheckBoxNode.centered(230, 170)
        pull.state = true
        pull.clickDelegate = {() => itemList.resetDownloadStats(); askForListRefresh()}
        addChild(pull)

        craft = CheckBoxNode.centered(230, 190)
        craft.state = true
        craft.clickDelegate = {() => itemList.resetDownloadStats(); askForListRefresh()}
        addChild(craft)

        partials = CheckBoxNode.centered(230, 210)
        addChild(partials)

        val ref = new MCButtonNode
        ref.position = Point(10, 185)
        ref.size = Size(50, 16)
        ref.text = "Refresh"
        ref.clickDelegate = {() => itemList.resetDownloadStats(); askForListRefresh()}
        addChild(ref)

        val req = new MCButtonNode
        req.position = Point(10, 205)
        req.size = Size(50, 16)
        req.text = "Submit"
        req.clickDelegate = {() => sendItemRequest()}
        addChild(req)

        val down = new MCButtonNode
        down.position = Point(95, 205)
        down.size = Size(16, 16)
        down.text = "-"
        down.clickDelegate = {() => countDown()}
        addChild(down)

        val up = new MCButtonNode
        up.position = Point(170, 205)
        up.size = Size(16, 16)
        up.text = "+"
        up.clickDelegate = {() => countUp()}
        addChild(up)

        val pageup = new MCButtonNode
        pageup.position = Point(85, 152)
        pageup.size = Size(16, 16)
        pageup.text = "-"
        pageup.clickDelegate = {() => itemList.pageUp()}
        addChild(pageup)

        val pagedown = new MCButtonNode
        pagedown.position = Point(180, 152)
        pagedown.size = Size(16, 16)
        pagedown.text = "+"
        pagedown.clickDelegate = {() => itemList.pageDown()}
        addChild(pagedown)

        val all = new MCButtonNode
        all.position = Point(190, 205)
        all.size = Size(24, 16)
        all.text = "All"
        all.clickDelegate = {() => if (itemList.getSelected != null) itemCount.text = String.valueOf(Math.max(1, itemList.getSelected.stackSize))}
        addChild(all)

        askForListRefresh()
    }

    private def sendItemRequest()
    {
        val count = itemCount.text
        if (count.isEmpty) return

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

    private def countUp()
    {
        var current = 0
        val s = itemCount.text
        if (s != null && !s.isEmpty) current = Integer.parseInt(s)

        val newCount =
            if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) current+10
            else current+1

        if (newCount < 999999999) itemCount.text = ""+newCount
    }

    private def countDown()
    {
        val s = itemCount.text
        val current = if (s.nonEmpty) Integer.parseInt(s) else 1

        val newCount =
            (if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) current-10
            else current-1) max 1

        itemCount.text = ""+newCount
    }

    def receiveContentList(content:Map[ItemKey, Int])
    {
        itemList.setDisplayList(content.map(p => ItemKeyStack.get(p._1, p._2)).toVector.sorted)
    }
}

class GuiFirewallPipe(slots:Container, pipe:RoutedFirewallPipe) extends NodeGui(slots, 276, 200)
{
    private[this] class SelectButton(x:Int, y:Int, f: => Boolean, desc:String) extends IconButtonNode
    {
        position = Point(x, y)
        size = Size(14, 14)
        tooltipBuilder = {_ += (EnumChatFormatting.GRAY+desc)}

        override def drawButton(mouseover:Boolean)
        {
            ResourceLib.guiExtras.bind()
            drawTexturedModalRect(x, y, if (f) 33 else 49, 134, 14, 14)
        }
    }

    override def onAddedToParent_Impl()
    {
        val excl = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                ResourceLib.guiExtras.bind()
                drawTexturedModalRect(position.x, position.y, if (pipe.filtExclude) 1 else 17, 102, 14, 14)
            }
        }
        excl.position = Point(134, 8)
        excl.size = Size(14, 14)
        excl.tooltipBuilder = {_ += (EnumChatFormatting.GRAY+"Items are "+
                (if (pipe.filtExclude) "blacklisted" else "whitelisted"))}
        excl.clickDelegate = {() => sendMessage("excl")}
        addChild(excl)


        val push = new SelectButton(183-7, 130-7, pipe.allowRoute, "Push routing")
        push.clickDelegate = {() => sendMessage("route")}
        addChild(push)

        val pull = new SelectButton(208-7, 130-7, pipe.allowBroadcast, "Pulling")
        pull.clickDelegate = {() => sendMessage("broad")}
        addChild(pull)

        val craft = new SelectButton(233-7, 130-7, pipe.allowCrafting, "Crafting")
        craft.clickDelegate = {() => sendMessage("craft")}
        addChild(craft)

        val cont = new SelectButton(258-7, 130-7, pipe.allowController, "Controller access")
        cont.clickDelegate = {() => sendMessage("cont")}
        addChild(cont)
    }

    def sendMessage(message:String)
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