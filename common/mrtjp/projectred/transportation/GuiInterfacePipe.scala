package mrtjp.projectred.transportation

import codechicken.lib.render.CCRenderState
import mrtjp.projectred.core.BasicGuiUtils
import mrtjp.projectred.core.inventory.GhostGuiContainer
import net.minecraft.inventory.Container
import net.minecraft.util.ResourceLocation

class GuiInterfacePipe(slots:Container, pipe:RoutedInterfacePipePart) extends GhostGuiContainer(slots, null, 176, 200)
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