package mrtjp.projectred.transportation

import mrtjp.projectred.core.BasicGuiUtils
import mrtjp.projectred.core.inventory.{GhostWidget, GhostGuiContainer}
import net.minecraft.inventory.Container
import codechicken.lib.render.CCRenderState

class GuiExtensionPipe(container:Container, id:String) extends GhostGuiContainer(container, null)
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
        CCRenderState.changeTexture(GhostGuiContainer.guiExtras)
        drawTexturedModalRect(134, 20, 1, 11, 16, 16)
    }
}