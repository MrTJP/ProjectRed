package mrtjp.projectred.transportation;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import net.minecraft.inventory.Container;
import codechicken.lib.render.CCRenderState;

public class GuiExtensionPipe extends GhostGuiContainer
{
    private final String id;
    
    public GuiExtensionPipe(Container container, String id)
    {
        super(container, null);
        this.id = id;
    }
    
    @Override
    public void drawBackground()
    {
        BasicGuiUtils.drawGuiBackGround(mc, 0, 0, xSize, ySize, zLevel, false);
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 84);
        
        fontRenderer.drawString("Extension ID:", 10, 10, 0xff000000);
        
        int i = 0;
        for (String s : id.split("-"))
            fontRenderer.drawString(s, 10, 25 + 10*i++, 0xff000000);

        BasicGuiUtils.drawSlotBackground(mc, 133, 19);
        BasicGuiUtils.drawSlotBackground(mc, 133, 49);
        
        CCRenderState.changeTexture(RL_extras);
        drawTexturedModalRect(134, 20, 1, 11, 16, 16);
    }
}
