package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.FontUtils
import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.core.BasicGuiUtils
import mrtjp.projectred.core.PRColors
import mrtjp.projectred.core.inventory.GhostGuiContainer
import mrtjp.projectred.core.inventory.JWidgetButton
import mrtjp.projectred.core.utils.Pair2
import net.minecraft.inventory.Container
import org.lwjgl.opengl.GL11
import net.minecraft.client.gui.Gui
import net.minecraft.util.ResourceLocation

class GuiCraftingPipe(container:Container, pipe:RoutedCraftingPipePart) extends GhostGuiContainer(container, null, 176, 220)
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
        CCRenderState.changeTexture(GhostGuiContainer.guiExtras)

        import scala.collection.JavaConversions._
        for (p <- BasicGuiUtils.createSlotArray(8, 108, 9, 1, 0, 0))
        {
            GL11.glColor4f(1, 1, 1, 1)
            drawTexturedModalRect(p.getValue1, p.getValue2, 1, 11, 16, 16)
            val x = p.getValue1 + 4
            val y = p.getValue2 - 2
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
            val x = p.getValue1 - 5
            val y = p.getValue2 - 2
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