package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.BlockCoord
import java.util.ArrayList
import java.util.Collections
import java.util.List
import java.util.Map
import mrtjp.projectred.core.BasicGuiUtils
import mrtjp.projectred.core.PRColors
import mrtjp.projectred.core.inventory._
import mrtjp.projectred.core.utils.ItemKey
import mrtjp.projectred.core.utils.ItemKeyStack
import org.lwjgl.input.Keyboard
import java.util

class GuiRequester(pipe:IWorldRequester) extends GhostGuiContainer(280, 230)
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
            val packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Request_submit)
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
        val packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Request_listRefresh)
        packet.writeCoord(new BlockCoord(pipe.getContainer.tile))
        packet.writeBoolean(pull.getChecked)
        packet.writeBoolean(craft.getChecked)
        packet.sendToServer()
    }

    private def sendAction(ident:String)
    {
        val packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Request_action)
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